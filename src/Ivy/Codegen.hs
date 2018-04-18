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
import           Control.Lens hiding (Context, assign, index, op, contexts)
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
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types
import           Ivy.Crypto.Keccak (keccak256)
import           Ivy.EvmAPI.API
import           Ivy.Syntax
import Ivy.AbiBridge
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

binOp :: (OpcodeM m, MemoryM m) => PrimType -> m () -> Integer -> Integer -> m Operand
binOp t op left right = do
  load right
  load left
  op
  addr <- alloc (sizeof t)
  push addr
  store
  return (Operand t addr)

executeBlock :: CodegenM m => Block -> m ()
executeBlock (Block stmts) = do
  createCtx
  mapM_ codegenStmt stmts
  popCtx

assign
  :: (MonadError CodegenError m, ContextM m, TcM m, MemoryM m)
  => PrimType
  -> Name
  -> Integer
  -> m ()
assign tyR name addr =
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (TextDetails "assignment"))
    Decl tyL -> do
      checkTyEq name tyL tyR
      updateCtx (M.update (const (Just (tyL, VarAddr (Just addr)))) name)
    Def tyL oldAddr -> do
      checkTyEq name tyL tyR
      storeAddressed addr oldAddr
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

  load predAddr
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
      load predAddr'

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
    Def (TArray _length aTy) oldAddr -> do
      checkTyEq name aTy tyR

      load addr

      load iAddr
      push32 (sizeInt (sizeof aTy))
      mul
      push32 oldAddr
      add
      store
    Def other _ -> throwError $ InternalError $ "codegenStmt ArrAssignment: non-array type is in symbol table as a definition for ArrAssignment code generation: " <> show other

codegenStmt (SVarDecl ty name) =
  declVar ty name

codegenStmt (SDeclAndAssignment ty name val) = do
  codegenStmt (SVarDecl ty name)
  codegenStmt (SAssignment name val)

codegenStmt (SIf ePred bodyBlock) = do
  Operand tyPred addrPred <- codegenExpr ePred
  checkTyEq "if_expr" tyPred TBool

  load addrPred
  iszero -- Negate for jumping condition

  -- offset <- estimateOffset bodyBlock
  rec push32 ifOut -- +3 because of the following `PC`, `ADD` and `JUMPI` instructions.)
      jumpi

      void $ executeBlock bodyBlock
      ifOut <- jumpdest
  pure ()

codegenStmt (SIfThenElse ePred trueBlock falseBlock) = do
  Operand tyPred addrPred <- codegenExpr ePred
  checkTyEq "if_else_expr" tyPred TBool

  load addrPred
  iszero -- Negate for jumping condition
  -- trueOffset <- estimateOffset trueBlock
  rec -- let trueJumpDest = pcCosts [PC, ADD, JUMPI, PUSH32 0, PC, ADD, JUMP] + trueOffset
      push32 trueDest
      jumpi

      executeBlock trueBlock
      -- falseOffset <- estimateOffset falseBlock

      -- let falseJumpDest = pcCosts [PC, ADD, JUMP, JUMPDEST] + falseOffset
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

data TOperand a = TOperand Addr

registerFunctionArgs :: forall m. (CodegenM m) => [(PrimType, Name)] -> m ()
registerFunctionArgs = traverse_ register_arg
  where
    register_arg :: (PrimType, Name) -> m ()
    register_arg (ty, name) = do
      addr <- alloc (sizeof ty)
      declVar ty name
      assign ty name addr

resetMemory :: CodegenM m => m ()
resetMemory = do
  memory .= initMemory
  memPointer .= 0

sigToKeccak256 :: forall m b. (MonadError CodegenError m, Read b, Num b) => FunSig -> m (Maybe b)
sigToKeccak256 (FunSig _ name args) = do
  argRep <- show_args
  pure $ keccak256 (name <> "(" <> argRep <> ")")
    where
      show_args :: m String
      show_args = intercalate "," <$> (mapM show_arg args)

      show_arg :: (PrimType, Name) -> m String
      show_arg (ty, _) =
        case toAbiTy ty of
          Left err -> throwError $ InternalError err
          Right abiTy -> pure $ show abiTy

codegenFunDef :: CodegenM m => FunStmt -> m ()
codegenFunDef (FunStmt signature@(FunSig _mods name args) block retTyAnnot) = do
  sigToKeccak256 signature >>= \case
    Nothing -> throwError $ InternalError $ "Could not take the keccak256 hash of the function name: " <> name
    Just fnNameHash -> do
      resetMemory -- TODO: remove this after we get persistence
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

          -- Store parameters
          storeParameters args

          -- Function body
          funPc <- use pc
          Operand retTy retAddr <- executeFunBlock block
          checkTyEq "function definition" retTyAnnot retTy
          functionOut <- jumpdest
      updateCtx (M.insert name (TFun retTy, FunAddr funPc retAddr))
      return ()
    where
      executeFunBlock :: forall m. CodegenM m => Block -> m Operand
      executeFunBlock (Block stmts) = do
        createCtx
        *> registerFunctionArgs args
        *> go stmts
        <* popCtx
        where
          go :: [Stmt] -> m Operand
          go []             = throwError NoReturnStatement
          go [SReturn expr] = do
            operand@(Operand _ty addr) <- codegenExpr expr
            push32 (addr + 0x20) -- TODO: ASSUMPTION: uint32
            push32 addr
            op_return
            return operand
          go (stmt:xs)      = codegenStmt stmt >> go xs

      -- | 0x04 magic number is the number of bytes CALLDATALOAD spends
      -- for determining function name. It's all parameter data after
      -- 0x04 bytes, if any.
      storeParameters :: forall m. CodegenM m => [(PrimType, Name)] -> m ()
      storeParameters = foldM_ storeParamsFold (0x04, 0x00)

      storeParamsFold :: forall m. CodegenM m => (Integer, Integer) -> (PrimType, Name) -> m (Integer, Integer)
      storeParamsFold (paramOffset, memOffset) (_ty, _name) =
           push32 paramOffset
        *> calldataload
        *> push32 memOffset
        *> mstore
        $> (paramOffset + 0x20, memOffset + 0x20) -- TODO: ASSUMPTION: uint32

takeArgsToContext :: forall m. CodegenM m => String -> [(PrimType, String, Integer)] -> [Operand] -> m ()
takeArgsToContext funcName registryArgs callerArgs = do
  unless (length registryArgs == length callerArgs) $
    throwError (FuncArgLengthMismatch funcName (length registryArgs) (length callerArgs))
  mapM_ bindArg (zip registryArgs callerArgs)
    where
      bindArg :: ((PrimType, String, Integer), Operand) -> m ()
      bindArg ((registryTy, registryName, registryAddr), Operand callerTy callerAddr) = do
        checkTyEq registryName registryTy callerTy
        storeAddressed callerAddr registryAddr

codegenFunCall :: CodegenM m => String -> [Expr] -> m Operand
codegenFunCall name args = do
  registry <- use funcRegistry
  case M.lookup name registry of
    Nothing -> throwError (VariableNotDeclared name (TextDetails "Function not declared"))
    Just registryArgs -> takeArgsToContext name registryArgs =<< mapM codegenExpr args

  lookupFun name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (ExprDetails (EFunCall name args)))
    FunDef retTy funAddr retAddr -> do
      -- Preparing checkpoint
      rec push32 funcDest

          -- Jumping to function
          push32 funAddr
          jump
          funcDest <- jumpdest

      return (Operand retTy retAddr)

codegenExpr :: forall m. CodegenM m => Expr -> m Operand
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
  return (Operand TInt addr)

codegenExpr (EChar val) = do
  addr <- alloc (sizeof TChar)
  storeVal (fromIntegral (ord val)) addr
  return (Operand TChar addr)

codegenExpr (EBool val) = do
  addr <- alloc (sizeof TBool)
  storeVal (boolToInt val) addr
  return (Operand TBool addr)

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

codegenExpr (EArray len elemExprs) = do
  elems <- mapM codegenExpr elemExprs
  elemTy <- testOperandsSameTy elems
  addr <- allocBulk len (sizeof elemTy)
  return (Operand (TArray len elemTy) addr)
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
  -- => Integer
  -- ^ Offset caused by initPhase.
  => [FunStmt]
  -> m ()
bodyPhase = traverse_ codegenFunDef
