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
{-# LANGUAGE ViewPatterns               #-}

module Cenary.Codegen where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens hiding (Context, assign, contexts, index, op)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import           Data.Char (ord)
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Prelude hiding (EQ, GT, LT, div, exp, log, lookup, mod, pred,
                          until)
--------------------------------------------------------------------------------
import           Cenary.Codegen.Procedures
import           Cenary.Codegen.Register
import           Cenary.Codegen.Types
import           Cenary.Crypto.Keccak (keccak256)
import           Cenary.EvmAPI.AbiBridge
import           Cenary.EvmAPI.API
import           Cenary.Syntax
import           Cenary.Codegen.Context (newEmptyCtx)
import qualified Cenary.Codegen.MappingOrder as MO
import           Cenary.Codegen.ContextM
--------------------------------------------------------------------------------

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
    Decl tyL scope -> do
      checkTyEq name tyL tyR
      addr <- alloc scope
      push32 addr
      store' scope
      ctxDefineVar name addr
    Def tyL oldAddr scope -> do
      checkTyEq name tyL tyR
      push32 oldAddr
      store' scope

declVar
  :: (MonadState CodegenState m, ContextM m, MonadError CodegenError m, MemoryM m)
  => PrimType
  -> Name
  -> Scope
  -> m ()
declVar ty name scope =
  lookup name >>= \case
    Decl _ _ -> throwError (VariableAlreadyDeclared name)
    Def _ _ _ -> throwError (VariableAlreadyDeclared name)
    NotDeclared -> do
      ctxDeclareVar name ty scope

codegenStmt :: forall m. CodegenM m => Stmt -> m ()
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

codegenStmt (SMapAssignment name key valExpr) = do
  Operand tyR <- codegenExpr valExpr
  -- [RHSVal]
  lookup name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (TextDetails "SMapAssignment"))
    Decl _ _ ->
      throwError $ InternalError $ "No mapping should be in Decl state but " <> name <> " is."
    Def (TMap tyAnnotKey tyAnnotVal) _ scope -> do
      when (scope /= Global) $ throwError $ InternalError "Persistence of mappings should be Permanent, not Temporary"
      checkTyEq name tyAnnotVal tyR
      Operand tyKey <- codegenExpr key
      -- [RHSVal, KeyVal]
      checkTyEq (name <> "_key") tyAnnotKey tyKey
      lookupMappingOrder name >>= \case
        Nothing ->
          throwError $ InternalError $ "Map named " <> name <> " does not have a order record."
        Just order -> do
          -- [RHSVal, KeyVal]
          applyHashingFunction order tyKey -- [RHSVal, SHA]
          store' Global
    Def other _ _ ->
      throwError $ InternalError $ "A mapping identifier is saved to lookup table in a non-suitable type: " <> show other


codegenStmt stmt@(SArrAssignment name index val) = do
  offset <- use funcOffset

  Operand tyI <- codegenExpr index
  checkTyEq ("index_of_" <> name) TInt tyI

  -- [1]
  Operand tyR <- codegenExpr val

  -- [1, 5]
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name (StmtDetails stmt)
    Decl _tyL _ -> throwError (NonInitializedArrayAccess name)
    Def (TArray aTy) addr scope -> do
      checkTyEq name aTy tyR
      load addr scope -- [1, 5, 340]
      -- Size check
      dup3 -- [1, 5, 340, 1]
      dup2 -- [1, 5, 340, 1, 340]
      load' scope -- [1, 5, 340, 1, 3]
      branchIf
        offset
        (push32 0x01 >> swap1 >> sub >> lt)
        (stop)
      -- [1, 5, 340]
      inc 0x20 -- [1, 5, 360]
      dup3 -- [1, 5, 360, 1]
      push32 0x20 -- [1, 5, 360, 1, 20]
      mul -- [1, 5, 360, 20]
      add -- [1, 5, 380]
      mstore -- [1]
      pop -- []
    Def ty _ _ -> throwError (IllegalArrAccess name ty)

codegenStmt (SVarDecl ty name) =
  declVar ty name Local

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

codegenStmt stmt@(SResize name sizeExpr) = do
  Operand tySize <- codegenExpr sizeExpr
  checkTyEq ("resize_" <> name) tySize TInt
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name (StmtDetails stmt)
    Decl _tyL _ -> throwError (NonInitializedArrayResize name)
    Def (TArray _) addr scope -> do
      push32 addr -- [NewSize, StackAddr]
      swap1 -- [StackAddr, NewSize]
      startResizingProcedure scope -- []
    Def ty _ _ -> do
      throwError $ CannotResizeNonArray ty

log :: CodegenM m => m ()
log = do
  -- [val]
  dup1 -- [val, val]
  shaAddr <- alloc Local
  push32 shaAddr -- [val, val, addr]
  mstore -- [val]
  push32 0x20 -- [val, 0x20]
  push32 shaAddr -- [val, 0x20, addr]
  log0 -- [val]

logContents :: Scope -> CodegenM m => m ()
logContents scope = do
  dup1 >> load' scope -- [(ArrLength + 2) * 0x20, HeapAddr, Size]
  log
  pop

logContents' :: Scope -> CodegenM m => m ()
logContents' scope = do
  dup1 >> load' scope >> load' scope -- [(ArrLength + 2) * 0x20, HeapAddr, Size]
  log
  pop

putFnArgsToContext :: forall m t. (CodegenM m, Foldable t) => t FuncRegistryArgInfo -> m ()
putFnArgsToContext = traverse_ register_arg
  where
    register_arg :: FuncRegistryArgInfo -> m ()
    register_arg (FuncRegistryArgInfo ty name) = do
      declVar ty name Local
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
  ctxDefineFunc name retTy funPc
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
                addr <- alloc Local
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
      argAddr <- alloc Local
      -- Fill arg addresses with incoming values from the call
      push32 calldataOffset
      calldataload
      dup1
      push32 argAddr
      mstore
      pure (calldataOffset + 0x20, FuncRegistryArgInfo argTy argName : registryInfo) -- TODO: ASSUMPTION: uint32

createStackFrame :: CodegenM m => m ()
createStackFrame = do
  sp' <- use sp
  framePtrs %= (sp':)
  env %= (contexts %~ (newEmptyCtx:))

codegenFunCall :: CodegenM m => String -> [Expr] -> m Operand
codegenFunCall name args = do
  lookupFun name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (ExprDetails (EFunCall name args)))
    FunDef retTy funAddr -> do
      storeRegVal Reg_FunCall 0x01
      offset <- use funcOffset
      rec push32 (funcDest - offset)
          -- createStackFrame
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
    Decl _ _ -> throwError (VariableNotDefined name)
    Def ty addr scope -> do
      load addr scope
      return (Operand ty)

codegenExpr (EArrIdentifier name index) = do
  offset <- use funcOffset
  lookup name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (TextDetails "in array assignment"))
    Decl _ _ ->
      throwError (VariableNotDefined name)
    Def (TArray ty) addr scope -> do
      Operand indexTy <- codegenExpr index
      -- a[2]
      checkTyEq name TInt indexTy

      push32 addr
      load' scope -- [2, 160]

      -- Runtime check for array bounds
      dup2 -- [2, 160, 2]
      dup2 -- [2, 160, 2, 160]
      load' scope -- [2, 160, 2, 1]
      branchIf
        offset
        (push32 0x01 >> swap1 >> sub >> lt)
        (stop)

      -- [2, 160]
      -- Get value at index
      inc 0x20
      swap1
      push32 0x20
      mul
      add
      load' scope

      return (Operand ty)
    Def ty _ _ -> throwError (IllegalArrAccess name ty)

codegenExpr (EInt val) = do
  push32 val
  return (Operand TInt)

codegenExpr (EChar (fromIntegral . ord -> val)) = do
  push32 val
  return (Operand TChar)

codegenExpr (EBool val) = do
  push32 (bool_to_int val)
  return (Operand TBool)
  where
    bool_to_int True  = 1
    bool_to_int False = 0

codegenExpr (EBinop binop expr1 expr2) = do
  Operand ty2 <- codegenExpr expr2
  Operand ty1 <- codegenExpr expr1
  operation <- case (ty1, ty2) of
    (TInt, TInt) -> pure $
      case binop of
        OpAdd -> add $> Operand TInt
        OpMul -> mul $> Operand TInt
        OpSub -> sub $> Operand TInt
        OpDiv -> div $> Operand TInt
        OpMod -> mod $> Operand TInt
        OpGt  -> gt  $> Operand TBool
        OpLt  -> lt  $> Operand TBool
        OpEq  -> eq  $> Operand TBool
    _ -> throwError $ WrongOperandTypes ty1 ty2
  operation

-- int[] a;
-- a = {1, 2, 3, 4};
codegenExpr (EArray elemExprs) = do
  elems <- mapM codegenExpr (reverse elemExprs)
  elemTy <- testOperandsSameTy elems
  arrAddr <- alloc Local

  heapBegin <- use heapSpaceBegin
  push32 heapBegin -- [3, 2, 1, 360]
  mload -- [3, 2, 1, 420]

  -- ** Tell its new address to our newly created array
  push32 arrAddr -- [3, 2, 1, 420, 40]
  dup2 -- [3, 2, 1, 420, 40, 420]
  swap1 -- [3, 2, 1, 420, 420, 40]
  mstore -- [3, 2, 1, 420]

  -- ** Actually store the elements
  dup1 -- [3, 2, 1, 420, 420]
  push32 (fromIntegral (length elemExprs)) -- [3, 2, 1, 420, 420, 3]
  swap1 -- [3, 2, 1, 420, 3, 420]
  mstore -- [3, 2, 1, 420]
  inc 0x20 -- [3, 2, 1, 440]
  replicateM_ (length elemExprs) $ do
    swap1 -- [3, 2, 440, 1]
    dup2 -- [3, 2, 440, 1, 440]
    mstore -- [3, 2, 440]
    inc 0x20 -- [3, 2, 460]
  -- [500]

  -- Update the heap end
  push32 heapBegin -- [500, 360]
  mstore -- []

  push32 arrAddr
  mload
  return (Operand (TArray elemTy))
  where
    testOperandsSameTy :: [Operand] -> m PrimType
    testOperandsSameTy [] = throwError EmptyArrayValue
    testOperandsSameTy (x:xs) =
      testOperandsHasTy (x ^. operandType) xs

    testOperandsHasTy :: PrimType -> [Operand] -> m PrimType
    testOperandsHasTy ty [] = return ty
    testOperandsHasTy ty (x:xs) =
      if x ^. operandType == ty
        then testOperandsHasTy ty xs
        else throwError (ArrayElementsTypeMismatch ty (x ^. operandType))

codegenExpr (EMapIdentifier name key) = do
  lookup name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (TextDetails "EMapIdentifier"))
    Decl _ _ ->
      throwError $ InternalError $ "No mapping should be in Decl state but " <> name <> " is."
    Def (TMap tyAnnotKey tyAnnotVal) _ scope -> do
      Operand tyKey <- codegenExpr key
      checkTyEq name tyAnnotKey tyKey
      lookupMappingOrder name >>= \case
        Nothing ->
          throwError $ InternalError $ "Map named " <> name <> " does not have a order record."
        Just order -> do
          -- [KeyValue]
          applyHashingFunction order tyKey -- [SHA]
          load' scope
          return (Operand tyAnnotVal)
    Def other _ _ ->
      throwError $ InternalError $ "A mapping identifier is saved to lookup table in a non-suitable type: " <> show other

lookupMappingOrder
  :: MonadState CodegenState m
  => Name
  -> m (Maybe Integer)
lookupMappingOrder name =
  MO.lookup name <$> use mappingOrder

codegenPhases :: CodegenM m => AST -> m ()
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

codegenGlobalStmt :: CodegenM m => GlobalStmt -> m ()
codegenGlobalStmt (GlobalFunc funStmt) = codegenFunDef funStmt
codegenGlobalStmt (GlobalDecl stmt) =
  case stmt of
    SVarDecl ty name -> declVar ty name Global
    -- TODO: Make this a user error, not an internal one
    _ -> throwError $ InternalError "Only declarations are allowed at global scope"

bodyPhase
  :: CodegenM m
  => AST
  -> m ()
bodyPhase stmts = do
  -- We use first 32 bytes (between 0x00 and 0x20) to determine
  -- whether a function is called internally or from outside
  allocRegisters
  rec
      heapSpaceBegin .= heapSpaceBegin'
      push32 (heapSpaceBegin' + 0x20)
      push32 heapSpaceBegin'
      mstore
      traverse_ codegenGlobalStmt stmts
      heapSpaceBegin' <- use sp
  pure ()

