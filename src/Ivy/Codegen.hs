{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens hiding (Context, assign, contexts, index, op)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import           Data.Char (ord)
import           Data.Foldable (traverse_)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Foldable (for_)
import           Prelude hiding (EQ, GT, LT, div, exp, log, lookup, mod, pred,
                          until)
--------------------------------------------------------------------------------
import           Ivy.AbiBridge
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types
import           Ivy.Crypto.Keccak (keccak256)
import           Ivy.EvmAPI.API
import           Ivy.Syntax
import           Ivy.Register
--------------------------------------------------------------------------------

-- | Class of monads that are able to read and update context
class ContextM m where
  updateCtx :: (Context -> Context) -> m ()
  lookup :: String -> m (VariableStatus VarsVar)
  lookupFun :: String -> m (VariableStatus VarsFun)
  createCtx :: m ()
  popCtx :: m ()
  updateSig :: Sig -> m ()

instance ContextM Evm where
  updateCtx f =
    env %= (contexts %~ updateCtx')
      where
        updateCtx' []       = []
        updateCtx' (ctx:xs) = f ctx : xs
  lookup name =
    go =<< _contexts <$> use env
    where
      go :: [Context] -> Evm (VariableStatus VarsVar)
      go [] = return NotDeclared
      go (ctx:xs) =
        case M.lookup name ctx of
          Just (varTy, VarAddr varAddr) -> decideVar (varTy, varAddr)
          Just _ -> throwError $ InternalError $ "Another case for context (1)"
          Nothing -> go xs
        where
          decideVar (ty, Nothing)   = return (Decl ty)
          decideVar (ty, Just addr) = return (Def ty addr)
  lookupFun name =
    go =<< _contexts <$> use env
      where
        go [] = return NotDeclared
        go (ctx:xs) =
          case M.lookup name ctx of
            Just (TFun retTy, FunAddr funAddr) -> return (FunDef retTy funAddr)
            Just _ -> throwError $ InternalError $ "Another case for context (2)"
            Nothing -> go xs
  createCtx = env %= (contexts %~ (M.empty :))
  popCtx = env %= (contexts %~ tail)
  updateSig sig' = env %= (sig .~ sig')

-- | Class of monads that can perform typechecking
class TcM m where
  checkTyEq :: Name -> PrimType -> PrimType -> m ()

instance TcM Evm where
  checkTyEq name tyL tyR =
    unless (tyL == tyR) $ throwError $ TypeMismatch name tyR tyL

binOp :: (OpcodeM m, MemoryM m) => PrimType -> m () -> m Operand
binOp t op = do
  -- We should have addresses of left and right operands, in this order by now.
  op
  addr <- alloc (sizeof t)
  push addr
  store
  load addr
  return (Operand t)

executeBlock :: CodegenM m => Block -> m ()
executeBlock (Block stmts) = do
  createCtx
  mapM_ codegenStmt stmts
  popCtx

assignFromStack
  :: (MonadError CodegenError m, ContextM m, TcM m, MemoryM m, OpcodeM m)
  => PrimType
  -> Name
  -> m ()
assignFromStack tyR name =
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (TextDetails "assignment"))
    Decl tyL -> do
      checkTyEq name tyL tyR
      addr <- alloc (sizeof tyR)
      push32 addr
      mstore
      updateCtx (M.update (const (Just (tyL, VarAddr (Just addr)))) name)
    Def tyL oldAddr -> do
      checkTyEq name tyL tyR
      push32 oldAddr
      store

declVar
  :: (MonadState CodegenState m, ContextM m, MonadError CodegenError m, MemoryM m)
  => PrimType
  -> Name
  -> m ()
declVar ty name =
  lookup name >>= \case
    Decl _ -> throwError (VariableAlreadyDeclared name)
    Def _ _ -> throwError (VariableAlreadyDeclared name)
    NotDeclared -> do
      updateCtx (M.insert name (ty, VarAddr Nothing))

codegenStmt :: CodegenM m => Stmt -> m ()
codegenStmt (SWhile pred block) = do
  Operand predTy <- codegenExpr pred
  checkTyEq "index_of_while_pred" TBool predTy

  iszero -- Reversing this bit because we jump to outside of while initally if predicate is false

  offset <- use funcOffset
  rec push32 (whileOut - offset)
      jumpi

      -- Loop start
      loopStart <- jumpdest

      -- Prepare true value of current PC
      push32 (loopStart - offset)

      -- Code body
      executeBlock block

      -- Load predicate again
      Operand predTy' <- codegenExpr pred
      checkTyEq "index_of_while_pred" TBool predTy'

      -- Jump to destination back if target value is nonzero
      swap1
      jumpi
      whileOut <- jumpdest
  pure ()

codegenStmt (SAssignment name val) = do
  Operand tyR <- codegenExpr val
  assignFromStack tyR name

-- codegenStmt stmt@(SArrAssignment name index val) = do
--   Operand tyI iAddr <- codegenExpr index
--   checkTyEq ("index_of_" <> name) TInt tyI
--   Operand tyR addr <- codegenExpr val
--   lookup name >>= \case
--     NotDeclared -> throwError $ VariableNotDeclared name (StmtDetails stmt)
--     Decl _tyL -> throwError (NonInitializedArrayAccess name)
--     Def (TArray _length aTy) oldAddr -> do
--       checkTyEq name aTy tyR

--       load addr

--       load iAddr
--       push32 (sizeInt (sizeof aTy))
--       mul
--       push32 oldAddr
--       add
--       store
--     Def other _ ->
--         throwError
--       $ InternalError
--       $ "codegenStmt ArrAssignment: non-array type is in symbol table"
--       <> "as a definition for ArrAssignment code generation: "
--       <> show other

codegenStmt (SVarDecl ty name) =
  declVar ty name

codegenStmt (SDeclAndAssignment ty name val) = do
  codegenStmt (SVarDecl ty name)
  codegenStmt (SAssignment name val)

codegenStmt (SIf ePred bodyBlock) = do
  Operand tyPred <- codegenExpr ePred
  checkTyEq "if_expr" tyPred TBool
  offset <- use funcOffset

  -- Loving this syntax style, should switch to lisp maybe
  branchIf
    (offset)
    (pure ())
    (void (executeBlock bodyBlock))
  pure ()

codegenStmt (SIfThenElse ePred trueBlock falseBlock) = do
  Operand tyPred <- codegenExpr ePred
  checkTyEq "if_else_expr" tyPred TBool

  offset <- use funcOffset
  branchIfElse (offset)
               (pure ())
               (executeBlock trueBlock)
               (executeBlock falseBlock)

codegenStmt (SReturn retExpr) =
  void (codegenExpr retExpr)

codegenStmt (SExpr expr) = void (codegenExpr expr)

-- | This type alias will be used for top-level codegen, since
-- at top level we use all contexts
type CodegenM m = (OpcodeM m, MonadState CodegenState m, MemoryM m, MonadError CodegenError m, ContextM m, TcM m, MonadFix m)

putFnArgsToContext :: forall m t. (CodegenM m, Foldable t) => t FuncRegistryArgInfo -> m ()
putFnArgsToContext = traverse_ register_arg
  where
    register_arg :: FuncRegistryArgInfo -> m ()
    register_arg (FuncRegistryArgInfo ty name) = do
      declVar ty name
      assignFromStack ty name

sigToKeccak256 :: forall m b. (MonadError CodegenError m, Read b, Num b) => FunSig -> m b
sigToKeccak256 (FunSig _ name args) = do
  argRep <- show_args
  let fnRep = name <> "(" <> argRep <> ")"
  case keccak256 fnRep of
    Nothing -> throwError $ InternalError $ "Could not take the keccak256 hash of the function name: " <> name
    Just hash -> pure hash
  where
    show_args :: m String
    show_args = intercalate "," <$> (mapM show_arg args)

    show_arg :: (PrimType, Name) -> m String
    show_arg (ty, _) =
      case toAbiTy ty of
        Left err    -> throwError $ InternalError err
        Right abiTy -> pure $ show abiTy

codegenFunDef :: CodegenM m => FunStmt -> m ()
codegenFunDef (FunStmt signature@(FunSig _mods name args) block retTyAnnot) = do
  fnNameHash <- sigToKeccak256 signature
  rec
      -- Function's case statement. If name does not match, we don't enter to this function.
      offset <- use funcOffset
      push4 fnNameHash
      push1 0xe0
      push1 0x02
      exp
      push1 0x00
      calldataload
      div
      eq
      iszero
      push32 (functionOut - offset)
      jumpi

      funPc <- jumpdest

      -- Crete context in which function arguments and its local variables will live
      createCtx

      -- Store parameters
      addressedArgs <- storeParameters

      -- Put function arguments to newly created context
      putFnArgsToContext (reverse addressedArgs)

      -- Function body (function bodies leave an element on stack, which is the address of the return value)
      Operand retTy <- executeFunBlock block

      -- Pop the function's context
      popCtx

      checkTyEq "function definition" retTyAnnot retTy
      functionOut <- jumpdest
  updateCtx (M.insert name (TFun retTy, FunAddr funPc))
  return ()
  where
    executeFunBlock :: forall m. CodegenM m => Block -> m Operand
    executeFunBlock (Block stmts) =
      go stmts
      where
        go :: [Stmt] -> m Operand
        go []             = throwError NoReturnStatement
        go [SReturn expr] = do
          operand@(Operand _ty) <- codegenExpr expr
          offset <- use funcOffset

          let internalCall = do
                swap1
                jump
              externalCall = do
                addr <- alloc (sizeof TInt)
                push32 addr
                mstore
                push32 (addr + 0x20)
                push32 addr
                op_return
                
          branchIfElse (offset)
                       (loadReg Reg_FunCall)
                       (internalCall)
                       (externalCall)

          return operand
        go (stmt:xs)      = codegenStmt stmt >> go xs

    -- | 0x04 magic number is the number of bytes CALLDATALOAD spends
    -- for determining function name. It's all parameter data after
    -- 0x04 bytes, if any.
    storeParameters :: forall m. CodegenM m => m [FuncRegistryArgInfo]
    storeParameters = do
      offset <- use funcOffset
      rec loadReg Reg_FunCall
          push32 (fillingEnd - offset)
          jumpi

          (_, registryArgInfo) <- foldM storeParamsFold (0x04, []) args
          funcRegistry %= (argsAddresses %~ M.insert name registryArgInfo)

          fillingEnd <- jumpdest
      pure registryArgInfo

    storeParamsFold :: forall m. CodegenM m => (Integer, [FuncRegistryArgInfo]) -> (PrimType, Name) -> m (Integer, [FuncRegistryArgInfo])
    storeParamsFold (calldataOffset, registryInfo) (argTy, argName) = do
      argAddr <- alloc (sizeof argTy)
      -- Fill arg addresses with incoming values from the call
      push32 calldataOffset
      calldataload
      dup1
      push32 argAddr
      mstore
      pure (calldataOffset + 0x20, FuncRegistryArgInfo argTy argName : registryInfo) -- TODO: ASSUMPTION: uint32

branchIf :: (OpcodeM m, MonadState CodegenState m, MonadFix m) => Integer -> m () -> m () -> m ()
branchIf offset loadPred ifComp = do
  rec loadPred
      iszero -- Jump if predicate is zero
      push32 (branchEnd - offset)
      jumpi
      ifComp
      branchEnd <- jumpdest
  pure ()

branchIfElse :: (OpcodeM m, MonadState CodegenState m, MonadFix m) => Integer -> m () -> m () -> m () -> m ()
branchIfElse offset loadPred ifComp elseComp = do
  rec loadPred
      push32 (elseEndIfBegin - offset)
      jumpi
      elseComp
      push32 (branchEnd - offset)
      jump
      elseEndIfBegin <- jumpdest
      ifComp
      branchEnd <- jumpdest
  pure()

codegenFunCall :: CodegenM m => String -> [Expr] -> m Operand
codegenFunCall name args = do
  lookupFun name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (ExprDetails (EFunCall name args)))
    FunDef retTy funAddr -> do
      storeRegVal Reg_FunCall 0x01
      offset <- use funcOffset
      rec push32 (funcDest - offset)
          push_func_args name
          push32 (funAddr - offset)
          jump
          funcDest <- jumpdest
      storeRegVal Reg_FunCall 0x00
      -- We already have function's return address on top of the stack here.
      return (Operand retTy)
  where
  push_func_args :: forall m. CodegenM m => String -> m ()
  push_func_args funcName = do
    FuncRegistry registryMap <- use funcRegistry
    case M.lookup name registryMap of
      Nothing -> throwError (VariableNotDeclared name (TextDetails "Function not declared"))
      Just (reverse -> registryArgs) -> do
        callerArgs <- mapM codegenExpr args

        -- Caller and function arg length check
        unless (length registryArgs == length callerArgs) $
          throwError (FuncArgLengthMismatch funcName (length registryArgs) (length callerArgs))

        -- Type check on function arguments
        mapM_ tyCheckFuncArg (zip registryArgs callerArgs)
          where
            tyCheckFuncArg :: (FuncRegistryArgInfo, Operand) -> m ()
            tyCheckFuncArg (FuncRegistryArgInfo registryTy registryName, Operand callerTy) = do
              checkTyEq registryName registryTy callerTy

codegenExpr :: forall m. CodegenM m => Expr -> m Operand
codegenExpr (EFunCall name args) =
  codegenFunCall name args

codegenExpr expr@(EIdentifier name) =
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (ExprDetails expr))
    Decl _ -> throwError (VariableNotDefined name)
    Def ty addr -> do
      load addr
      return (Operand ty)

codegenExpr (EArrIdentifier name index) =
  lookup name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (TextDetails "in array assignment"))
    Decl _ ->
      throwError (VariableNotDefined name)
    Def (TArray _ ty) addr -> do
      Operand indexTy <- codegenExpr index
      checkTyEq name TInt indexTy
      push32 0x20
      mul
      inc addr
      mload
      return (Operand ty)
    Def ty _ -> throwError (IllegalArrAccess name ty)

codegenExpr (EInt val) = do
  push32 val
  return (Operand TInt)

codegenExpr (EChar (fromIntegral . ord -> val)) = do
  push32 val
  return (Operand TChar)

codegenExpr (EBool (boolToInt -> val)) = do
  push32 val
  return (Operand TBool)

codegenExpr (EBinop binop expr1 expr2) = do
  Operand ty2 <- codegenExpr expr2
  Operand ty1 <- codegenExpr expr1
  operation <- case (ty1, ty2) of
    (TInt, TInt) -> pure $
      case binop of
        OpAdd -> binOp TInt add
        OpMul -> binOp TInt mul
        OpSub -> binOp TInt sub
        OpDiv -> binOp TInt div
        OpMod -> binOp TInt mod
        OpGt  -> binOp TBool gt
        OpLt  -> binOp TBool lt
        OpEq  -> binOp TBool eq
    _ -> throwError $ WrongOperandTypes ty1 ty2
  operation

-- codegenExpr (EArray len elemExprs) = do
--   elems <- mapM codegenExpr elemExprs
--   elemTy <- testOperandsSameTy elems
--   beginAddr <- allocBulk len (sizeof elemTy)

--   -- Store parameters at their respective stack addresses
--   let size = sizeInt (sizeof elemTy)
--   let endAddr = beginAddr + size * (len-1)
--   for_ [endAddr,endAddr-size..beginAddr] $ \addr -> do
--     push32 addr
--     mstore

--   return (Operand (TArray len elemTy))
--   where
--     testOperandsSameTy :: [Operand] -> m PrimType
--     testOperandsSameTy [] = throwError EmptyArrayValue
--     testOperandsSameTy (x:xs) =
--       testOperandsHasTy (x ^. operandType) xs

--     testOperandsHasTy :: PrimType -> [Operand] -> m PrimType
--     testOperandsHasTy ty [] = return ty
--     testOperandsHasTy ty (x:xs) =
--       if x ^. operandType == ty
--         then testOperandsHasTy ty xs
--         else throwError (ArrayElementsTypeMismatch ty (x ^. operandType))

codegenPhases :: CodegenM m => [FunStmt] -> m ()
codegenPhases functions = do
  rec initPhase (afterFunctions - afterInit)
      afterInit <- use pc
      funcOffset .= afterInit
      bodyPhase functions
      afterFunctions <- use pc
  pure ()

initPhase :: Integer -> CodegenM m => m ()
initPhase functionCosts = do
  rec push32 functionCosts
      dup1
      push32 initCost
      push32 0x00
      codecopy
      push32 0x00
      op_return
      stop
      initCost <- use pc
  pure ()

bodyPhase
  :: CodegenM m
  => [FunStmt]
  -> m ()
bodyPhase stmts = do
  -- We use first 32 bytes (between 0x00 and 0x20) to determine 
  -- whether a function is called internally or from outside
  allocRegisters

  traverse_ codegenFunDef stmts
