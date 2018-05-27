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
  updateCtx :: (Context -> (Bool, Context)) -> m ()
  lookup :: String -> m (VariableStatus VarsVar)
  lookupFun :: String -> m (VariableStatus VarsFun)
  createCtx :: m ()
  popCtx :: m ()
  updateSig :: Sig -> m ()
  ctxDeclareVar :: Name -> PrimType -> VariablePersistence -> m ()
  ctxDefineVar :: Name -> Integer -> m ()
  ctxDefineFunc :: Name -> PrimType -> Integer -> m ()

instance ContextM Evm where
  updateCtx f' =
    env %= (contexts %~ updateCtx' f')
      where
        updateCtx' _ []       = []
        updateCtx' f (ctx:xs) = case f ctx of
          (True, newCtx) -> newCtx : xs
          (False, oldCtx) -> oldCtx : updateCtx' f xs
  lookup name =
    go =<< _contexts <$> use env
    where
      go :: [Context] -> Evm (VariableStatus VarsVar)
      go [] = return NotDeclared
      go (ctx:xs) =
        case M.lookup name ctx of
          Just (varTy, VarAddr varAddr persistence) -> decideVar (varTy, varAddr) persistence
          Just _ -> throwError $ InternalError $ "Another case for context (1)"
          Nothing -> go xs
        where
          decideVar (ty, Nothing)   p = return (Decl ty p)
          decideVar (ty, Just addr) p = return (Def ty addr p)
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
  ctxDeclareVar name ty persistence =
    lookup name >>= \case
      NotDeclared -> do
        updateCtx ((True,) . M.insert name (ty, VarAddr Nothing persistence))
        case ty of
          TMap _ _ -> do
            addr <- alloc persistence
            createMappingOrder name
            ctxDefineVar name addr
          _ -> pure ()
        
      Decl _ _ ->
        throwError $ InternalError $ "TODO (2)"
      Def _ _ _ ->
        throwError $ InternalError $ "TODO (3)"
  ctxDefineVar name addr =
    lookup name >>= \case
      NotDeclared -> throwError (VariableNotDeclared name (TextDetails "ctxDefineVar"))
      Decl tyL persistence -> do
        updateCtx $ \ctx ->
          case M.lookup name ctx of
            Nothing ->
              (False, ctx)
            Just _ ->
              (True, M.update (const (Just (tyL, VarAddr (Just addr) persistence))) name ctx)
      Def _ _ _ -> throwError $ InternalError $ "TODO (1)"
  ctxDefineFunc name retTy funPc =
    updateCtx ((True,) . M.insert name (TFun retTy, FunAddr funPc))

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
    Decl tyL persistence -> do
      checkTyEq name tyL tyR
      addr <- alloc persistence
      push32 addr
      store' persistence
      ctxDefineVar name addr
    Def tyL oldAddr persistence -> do
      checkTyEq name tyL tyR
      push32 oldAddr
      store' persistence

declVar
  :: (MonadState CodegenState m, ContextM m, MonadError CodegenError m, MemoryM m)
  => PrimType
  -> Name
  -> VariablePersistence
  -> m ()
declVar ty name persistence =
  lookup name >>= \case
    Decl _ _ -> throwError (VariableAlreadyDeclared name)
    Def _ _ _ -> throwError (VariableAlreadyDeclared name)
    NotDeclared -> do
      ctxDeclareVar name ty persistence

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

codegenStmt (SMapAssignment _name _key _val) =
  undefined -- TODO: fill

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
    Def (TArray aTy) addr persistence -> do
      checkTyEq name aTy tyR
      load addr persistence -- [1, 5, 340]
      -- Size check
      dup3 -- [1, 5, 340, 1]
      dup2 -- [1, 5, 340, 1, 340]
      load' persistence -- [1, 5, 340, 1, 3]
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
  declVar ty name Temporary

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
    Def (TArray _) addr persistence -> do
      load addr persistence -- [NewSize, StackAddr]
      swap1 -- [StackAddr, NewSize]
      startResizingProcedure persistence -- [StackAddr]
      pop
    Def ty _ _ -> do
      throwError $ CannotResizeNonArray ty

-- | This type alias will be used for top-level codegen, since
-- at top level we use all contexts
type CodegenM m = (OpcodeM m, MonadState CodegenState m, MemoryM m, MonadError CodegenError m, ContextM m, TcM m, MonadFix m)

-- |
-- Input: [StackAddr, NewSize]
-- Output: [StackAddr]
startResizingProcedure
  :: forall m. (MemoryM m, OpcodeM m, MonadState CodegenState m, MonadFix m)
  => VariablePersistence
  -> m ()
startResizingProcedure persistence = do
  offset <- use funcOffset
  branchIfElse
    (offset)
    (do
      dup2 -- [StackAddr, NewSize, StackAddr]
      dup2 -- [StackAddr, NewSize, StackAddr, NewSize]
      swap1 -- [StackAddr, NewSize, NewSize, StackAddr]
      load' persistence -- [StackAddr, NewSize, NewSize, OldHeapAddr]
      load' persistence -- [StackAddr, NewSize, NewSize, OldSize]
      lt -- [StackAddr, NewSize, OldSize < NewSize]
    )
    -- new size is bigger, allocate new array space
    -- [StackAddr, NewSize] | NewSize > OldSize
    (moveToNewArrAddr offset persistence)
    -- [StackAddr, NewSize] | NewSize <= OldSize
    (swap1 >> mstore) -- new size is smaller, just set the length identifier address
  where
    moveToNewArrAddr :: Integer -> VariablePersistence -> m ()
    moveToNewArrAddr offset persistence = do
      -- [StackAddr, NewSize]
      rec heapBegin <- use heapSpaceBegin
          push32 heapBegin
          load' persistence -- [StackAddr, NewSize, NewAddr]
          swap1 -- [StackAddr, NewAddr, NewSize]
          dup2 -- [StackAddr, NewAddr, NewSize, NewAddr]
          mstore -- [StackAddr, NewAddr]
          swap1 -- [NewAddr, StackAddr]
          dup1 >> mload -- [NewAddr, StackAddr, OldAddr]
          dup1 >> mload -- [NewAddr, StackAddr, OldAddr, OldSize]

          loopBegin <- jumpdest

          -- Test for loop
          dup1 -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize]
          (push32 0 >> lt) -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize > 0]
          iszero -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize <= 0]
          push32 (loopEnd - offset) -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize <= 0, loopEnd]
          jumpi -- [NewAddr, StackAddr, OldAddr, OldSize]

          -- Loop body
          -- [NewAddr, StackAddr, OldAddr, OldSize]
          dup1 -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize]
          dec 0x01 -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize - 1] (index indicated by oldSize)
          dup1 -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize - 1, oldSize - 1]
          (push32 0x20 >> mul) -- [NewAddr, StackAddr, OldAddr, OldSize, oldSize - 1, 0x20 * (OldSize - 1)]
          dup3 -- [NewAddr, StackAddr, OldAddr, OldSize, oldSize - 1, 0x20 * (OldSize - 1), oldAddr]
          add -- [NewAddr, StackAddr, OldAddr, OldSize, oldSize - 1, 0x20 * (OldSize - 1) + oldAddr]
          mload -- [NewAddr, StackAddr, OldAddr, OldSize, oldSize - 1, oldAddr[size-1]]
          swap1 -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], oldSize - 1]
          (push32 0x20 >> mul) -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], 0x20 * (oldSize - 1)]
          dup6 -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], 0x20 * (oldSize - 1), NewAddr]
          add -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], 0x20 * (oldSize - 1) + NewAddr]
          mstore -- [NewAddr, StackAddr, OldAddr, OldSize]
          dec 0x01 -- [NewAddr, StackAddr, OldAddr, OldSize - 1]

          -- Jump back to beginning of loop
          push32 (loopBegin - offset)
          jump

          -- End of the loop
          loopEnd <- jumpdest
          -- [NewAddr, StackAddr, OldAddr, 0]
          (pop >> pop) -- [NewAddr, StackAddr]
          swap1 -- [StackAddr, NewAddr]
          dup2 -- [StackAddr, NewAddr, StackAddr]
          mstore -- [StackAddr]
      pure ()

putFnArgsToContext :: forall m t. (CodegenM m, Foldable t) => t FuncRegistryArgInfo -> m ()
putFnArgsToContext = traverse_ register_arg
  where
    register_arg :: FuncRegistryArgInfo -> m ()
    register_arg (FuncRegistryArgInfo ty name) = do
      declVar ty name Temporary
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
                addr <- alloc Temporary
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
      argAddr <- alloc Temporary
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
    Decl _ _ -> throwError (VariableNotDefined name)
    Def ty addr persistence -> do
      load addr persistence
      return (Operand ty)

codegenExpr (EArrIdentifier name index) = do
  offset <- use funcOffset
  lookup name >>= \case
    NotDeclared ->
      throwError (VariableNotDeclared name (TextDetails "in array assignment"))
    Decl _ _ ->
      throwError (VariableNotDefined name)
    Def (TArray ty) addr persistence -> do
      Operand indexTy <- codegenExpr index
      -- a[2]
      checkTyEq name TInt indexTy

      push32 addr
      load' persistence -- [2, 160]

      -- Runtime check for array bounds
      dup2 -- [2, 160, 2]
      dup2 -- [2, 160, 2, 160]
      load' persistence -- [2, 160, 2, 1]
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
      load' persistence

      return (Operand ty)
    Def ty _ _ -> throwError (IllegalArrAccess name ty)

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

-- int[] a;
-- a = {1, 2, 3, 4};
codegenExpr (EArray elemExprs) = do
  elems <- mapM codegenExpr (reverse elemExprs)
  elemTy <- testOperandsSameTy elems
  arrAddr <- alloc Temporary

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
    Def (TMap tyAnnotKey tyAnnotVal) addr persistence -> do
      Operand tyKey <- codegenExpr key
      checkTyEq name tyAnnotKey tyKey
      lookupMappingOrder name >>= \case
        Nothing ->
          throwError $ InternalError $ "Map named " <> name <> " does not have a order record."
        Just order ->
          case tyKey of
            TInt -> do
              -- [StackAddr]
              load' persistence -- [Value]
              valAddr <- alloc persistence
              orderAddr <- alloc persistence
              push32 valAddr -- [Value, ValAddr]
              store' persistence -- []
              push32 order -- [Order]
              push32 orderAddr -- [Order, OrderAddr]
              store' persistence -- []
              push32 (valAddr + 0x40)
              push32 valAddr
              sha3
              load' persistence
              return (Operand tyAnnotVal)
            TArray _ ->  do
              -- [ArrAddr]
              stackAddr <- alloc persistence
              push32 stackAddr -- [ArrAddr, StackAddr]
              store' persistence -- []
              push32 stackAddr -- [StackAddr]

              dup1 >> load' persistence -- [StackAddr, ArrAddr]
              load' persistence -- [StackAddr, ArrLength]
              inc 0x01 -- [StackAddr, ArrLength + 1]
              dup1 -- [StackAddr, ArrLength + 1, ArrLength + 1]
              swap2 -- [ArrLength + 1, ArrLength + 1, StackAddr]
              swap1 -- [ArrLength + 1, StackAddr, ArrLength + 1]
              startResizingProcedure persistence -- [ArrLength + 1, StackAddr]
              swap1 >> dup2 -- [StackAddr, ArrLength + 1, StackAddr]
              load' persistence -- [StackAddr, ArrLength + 1, ArrAddr]
              dup2 -- [StackAddr, ArrLength + 1, ArrAddr, ArrLength + 1]
              push32 0x20 >> mul -- [StackAddr, ArrLength + 1, ArrAddr, 0x20 * (ArrLength + 1)]
              add -- [StackAddr, ArrLength + 1, ArrAddr + 0x20 * (ArrLength + 1)]
              push32 order -- [StackAddr, ArrLength + 1, ArrAddr + 0x20 * (ArrLength + 1), Order]
              swap1 -- [StackAddr, ArrLength + 1, Order, ArrAddr + 0x20 * (ArrLength + 1)]
              store' persistence -- [StackAddr, ArrLength + 1]
              push32 0x20 >> mul -- [StackAddr, (ArrLength + 1) * 0x20]
              swap1 -- [(ArrLength + 1) * 0x20, StackAddr]
              load' persistence -- [(ArrLength + 1) * 0x20, ArrAddr]
              swap1 >> dup2 -- [ArrAddr, (ArrLength + 1) * 0x20, ArrAddr]
              add -- [ArrAddr, (ArrLength + 1) * 0x20 + ArrAddr]
              inc 0x20 -- [ArrAddr, (ArrLength + 2) * 0x20 + ArrAddr]
              swap1 -- [(ArrLength + 2) * 0x20 + ArrAddr, ArrAddr]
              sha3 -- [SHA3]
              load' persistence -- [Value]
              return (Operand tyAnnotVal)

lookupMappingOrder
  :: MonadState CodegenState m
  => Name
  -> m (Maybe Integer)
lookupMappingOrder name =
  M.lookup name <$> use mappingOrder

createMappingOrder
  :: (MonadState CodegenState m, MonadError CodegenError m)
  => Name
  -> m ()
createMappingOrder name = do
  nextOrder <- nextMappingOrder <<+= 1
  currentMappingOrder <- use mappingOrder
  case M.lookup name currentMappingOrder of
    Nothing ->
      mappingOrder %= M.insert name nextOrder
    Just _ ->
      throwError $ InternalError $ "Multiple mapping order insert attempts for mapping named " <> name

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
    SVarDecl ty name -> declVar ty name Permanent

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
      heapSpaceBegin' <- use stackMemEnd
  pure ()
