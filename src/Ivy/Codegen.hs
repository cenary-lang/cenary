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
import           Prelude hiding (EQ, GT, LT, div, exp, log, lookup, mod, pred,
                          until)
--------------------------------------------------------------------------------
import           Ivy.AbiBridge
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types
import           Ivy.Crypto.Keccak (keccak256)
import           Ivy.EvmAPI.API
import           Ivy.Syntax
--------------------------------------------------------------------------------

-- | Class of monads that are able to read and update context
class ContextM m where
  updateCtx :: (Context m -> Context m) -> m ()
  lookup :: String -> m (VariableStatus m VarsVar)
  lookupFun :: String -> m (VariableStatus m VarsFun)
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
      go :: [Context f] -> Evm (VariableStatus f VarsVar)
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
            Just (TFun retTy, FunAddr funAddr retAddr) -> return (FunDef retTy funAddr retAddr)
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

binOp :: (OpcodeM m, MemoryM m) => PrimType -> m () -> OperandAddr m -> OperandAddr m -> m (Operand m)
binOp t op left right = do
  loadAddr right
  loadAddr left
  op
  addr <- alloc (sizeof t)
  push addr
  mstore
  return (mkOperand t addr)

executeBlock :: CodegenM m => Block -> m ()
executeBlock (Block stmts) = do
  createCtx
  mapM_ codegenStmt stmts
  popCtx

assign
  :: (MonadError CodegenError m, ContextM m, TcM m, MemoryM m)
  => PrimType
  -> Name
  -> OperandAddr m
  -> m ()
assign tyR name addr =
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (TextDetails "assignment"))
    Decl tyL -> do
      checkTyEq name tyL tyR
      updateCtx (M.update (const (Just (tyL, VarAddr (Just addr)))) name)
    Def tyL oldAddr -> do
      checkTyEq name tyL tyR
      store addr oldAddr
      updateCtx (M.update (const (Just (tyL, VarAddr (Just addr)))) name)

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
  Operand predTy predAddr <- codegenExpr pred
  checkTyEq "index_of_while_pred" TBool predTy

  loadAddr predAddr
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
      Operand predTy' predAddr' <- codegenExpr pred
      checkTyEq "index_of_while_pred" TBool predTy'
      loadAddr predAddr'

      -- Jump to destination back if target value is nonzero
      swap1
      jumpi
      whileOut <- jumpdest
  pure ()

codegenStmt (SAssignment name val) = do
  Operand tyR addr <- codegenExpr val
  assign tyR name addr

codegenStmt stmt@(SArrAssignment name index val) = do
  Operand tyI iAddr <- codegenExpr index
  checkTyEq ("index_of_" <> name) TInt tyI
  Operand tyR addr <- codegenExpr val
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name (StmtDetails stmt)
    Decl _tyL -> throwError (NonInitializedArrayAccess name)
    Def (TArray _length aTy) oldUnknownAddr -> do
      oldAddr <- enforceStackAddr oldUnknownAddr
      checkTyEq name aTy tyR

      loadAddr addr

      loadAddr iAddr
      push32 (sizeInt (sizeof aTy))
      mul
      push32 oldAddr
      add
      mstore
    Def other _ ->
        throwError
      $ InternalError
      $ "codegenStmt ArrAssignment: non-array type is in symbol table"
      <> "as a definition for ArrAssignment code generation: "
      <> show other

codegenStmt (SVarDecl ty name) =
  declVar ty name

codegenStmt (SDeclAndAssignment ty name val) = do
  codegenStmt (SVarDecl ty name)
  codegenStmt (SAssignment name val)

codegenStmt (SIf ePred bodyBlock) = do
  Operand tyPred addrPred <- codegenExpr ePred
  checkTyEq "if_expr" tyPred TBool
  offset <- use funcOffset
  -- Loving this syntax style, should switch to lisp maybe
  branchIf
    (offset)
    (loadAddr addrPred)
    (void (executeBlock bodyBlock))
  pure ()

codegenStmt (SIfThenElse ePred trueBlock falseBlock) = do
  Operand tyPred predAddr <- codegenExpr ePred
  checkTyEq "if_else_expr" tyPred TBool

  loadAddr predAddr
  iszero -- Negate for jumping condition
  rec
      push32 trueDest
      jumpi

      executeBlock trueBlock

      push32 falseDest
      jump

      trueDest <- jumpdest
      executeBlock falseBlock
      falseDest <- jumpdest
  pure ()

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
    register_arg (FuncRegistryArgInfo ty name addr) = do
      declVar ty name
      assign ty name (liftToOperandAddr addr)

liftToOperandAddr :: Integer -> OperandAddr f
liftToOperandAddr addr = OperandAddr (Left addr)

resetMemory :: CodegenM m => m ()
resetMemory = do
  memory .= initMemory
  memPointer .= 0

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

enforceStackAddr :: MonadError CodegenError m => OperandAddr m -> m Integer
enforceStackAddr (OperandAddr (Right _)) = throwError $ InternalError $ "enforceStackAddr: Right found"
enforceStackAddr (OperandAddr (Left addr)) = pure addr

codegenFunDef :: CodegenM m => FunStmt -> m ()
codegenFunDef (FunStmt signature@(FunSig _mods name args) block retTyAnnot) = do
  fnNameHash <- sigToKeccak256 signature
  -- resetMemory -- TODO: remove this after we get persistence
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

      -- Store parameters
      addressedArgs <- storeParameters

      -- Function body
      Operand retTy unknownRetAddr <- executeFunBlock addressedArgs block
      retAddr <- enforceStackAddr unknownRetAddr
      checkTyEq "function definition" retTyAnnot retTy
      functionOut <- jumpdest
  updateCtx (M.insert name (TFun retTy, FunAddr funPc retAddr))
  return ()
  where
    executeFunBlock :: forall m. CodegenM m => [FuncRegistryArgInfo] -> Block -> m (Operand m)
    executeFunBlock addressedArgs (Block stmts) = do
      createCtx
      *> putFnArgsToContext addressedArgs
      *> go stmts
      <* popCtx
      where
        go :: [Stmt] -> m (Operand m)
        go []             = throwError NoReturnStatement
        go [SReturn expr] = do
          operand@(Operand _ty unknownAddr) <- codegenExpr expr
          addr <- enforceStackAddr unknownAddr
          offset <- use funcOffset

          branchIfElse (offset)
                       (load 0x00)
                       (jump)
                       (push32 (addr + 0x20) >> push32 addr >> op_return)

          return operand
        go (stmt:xs)      = codegenStmt stmt >> go xs

    -- | 0x04 magic number is the number of bytes CALLDATALOAD spends
    -- for determining function name. It's all parameter data after
    -- 0x04 bytes, if any.
    storeParameters :: forall m. CodegenM m => m [FuncRegistryArgInfo]
    storeParameters = do
      offset <- use funcOffset
      rec load 0x00
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
      push32 argAddr
      mstore
      pure (calldataOffset + 0x20, FuncRegistryArgInfo argTy argName argAddr : registryInfo) -- TODO: ASSUMPTION: uint32

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
      elseEndIfBegin <- jumpdest
      ifComp
      branchEnd <- jumpdest
  pure()

codegenFunCall :: CodegenM m => String -> [Expr] -> m (Operand m)
codegenFunCall name args = do
  FuncRegistry registryMap <- use funcRegistry
  case M.lookup name registryMap of
    Nothing -> throwError (VariableNotDeclared name (TextDetails "Function not declared"))
    Just registryArgs -> write_func_args name registryArgs =<< mapM codegenExpr args

  lookupFun name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (ExprDetails (EFunCall name args)))
    FunDef retTy funAddr retAddr -> do
      storeVal 0x01 0x00
      offset <- use funcOffset
      rec push32 (funcDest - offset)
          push32 (funAddr - offset)
          jump
          funcDest <- jumpdest
      storeVal 0x00 0x00
      return (mkOperand retTy retAddr)
  where
  write_func_args :: forall m. CodegenM m => String -> [FuncRegistryArgInfo] -> [Operand m] -> m ()
  write_func_args funcName registryArgs callerArgs = do
    unless (length registryArgs == length callerArgs) $
      throwError (FuncArgLengthMismatch funcName (length registryArgs) (length callerArgs))
    mapM_ bindArg (zip registryArgs callerArgs)
      where
        bindArg :: (FuncRegistryArgInfo, Operand m) -> m ()
        bindArg (FuncRegistryArgInfo registryTy registryName registryAddr, Operand callerTy callerAddr) = do
          checkTyEq registryName registryTy callerTy
          storeOnStack callerAddr registryAddr

codegenExpr :: forall m. CodegenM m => Expr -> m (Operand m)
codegenExpr (EFunCall name args) =
  codegenFunCall name args

codegenExpr expr@(EIdentifier name) =
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (ExprDetails expr))
    Decl _ -> throwError (VariableNotDefined name)
    Def ty addr -> return (Operand ty addr)

codegenExpr (EArrIdentifier name index) =
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (TextDetails "in array assignment"))
    Decl _ -> throwError (VariableNotDefined name)
    Def (TArray _ ty) addr -> do
      Operand indexTy _ <- codegenExpr index
      checkTyEq name TInt indexTy
      return (Operand ty addr)
    Def ty _ -> throwError (IllegalArrAccess name ty)

codegenExpr (EInt val) = do
  addr <- alloc (sizeof TInt)
  storeVal val addr
  return (mkOperand TInt addr)

codegenExpr (EChar val) = do
  addr <- alloc (sizeof TChar)
  storeVal (fromIntegral (ord val)) addr
  return (mkOperand TChar addr)

codegenExpr (EBool val) = do
  addr <- alloc (sizeof TBool)
  storeVal (boolToInt val) addr
  return (mkOperand TBool addr)

codegenExpr (EBinop binop expr1 expr2) = do
  Operand ty1 left <- codegenExpr expr1
  Operand ty2 right <- codegenExpr expr2
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
  operation left right

codegenExpr EDynArr =
  let loader = pure ()
      storer = pure ()
  in
    pure $ mkHeapOperand (TDynArr TInt) loader storer

codegenExpr (EArr len elemExprs) = do
  elems <- mapM codegenExpr elemExprs
  elemTy <- testOperandsSameTy elems
  addr <- allocBulk len (sizeof elemTy)
  return (mkOperand (TArray len elemTy) addr)
  where
    testOperandsSameTy :: [Operand m] -> m PrimType
    testOperandsSameTy [] = throwError EmptyArrayValue
    testOperandsSameTy (x:xs) =
      testOperandsHasTy (x ^. operandType) xs

    testOperandsHasTy :: PrimType -> [Operand m] -> m PrimType
    testOperandsHasTy ty [] = return ty
    testOperandsHasTy ty (x:xs) =
      if x ^. operandType == ty
        then testOperandsHasTy ty xs
        else throwError (ArrayElementsTypeMismatch ty (x ^. operandType))


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
  _ <- alloc (sizeof TInt)

  traverse_ codegenFunDef stmts
