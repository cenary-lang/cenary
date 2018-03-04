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
import           Control.Arrow ((***))
import           Control.Lens hiding (Context, assign, index, op)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import           Data.Char (ord)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Prelude hiding (EQ, GT, LT, log, lookup, pred, until)
--------------------------------------------------------------------------------
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types
import           Ivy.EvmAPI.Instruction
import           Ivy.Syntax
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
    env %= (id *** updateCtx')
      where
        updateCtx' []       = []
        updateCtx' (ctx:xs) = f ctx : xs
  lookup name =
    go =<< snd <$> use env
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
    go =<< snd <$> use env
      where
        go [] = return NotDeclared
        go (ctx:xs) =
          case M.lookup name ctx of
            Just (TFun retTy, FunAddr funAddr retAddr) -> return (FunDef retTy funAddr retAddr)
            Just _ -> throwError $ InternalError $ "Another case for context (2)"
            Nothing -> go xs
  createCtx = env %= (id *** (M.empty :))
  popCtx = env %= (id *** tail)
  updateSig sig = env %= (const sig *** id)

-- | Class of monads that can perform typechecking
class TcM m where
  checkTyEq :: Name -> PrimType -> PrimType -> m ()

instance TcM Evm where
  checkTyEq name tyL tyR =
    unless (tyL == tyR) $ throwError $ TypeMismatch name tyR tyL

binOp :: (OpcodeM m, MemoryM m) => PrimType -> Instruction -> Integer -> Integer -> m Operand
binOp t instr left right = do
  load (sizeof TInt) right
  load (sizeof TInt) left
  op instr
  addr <- alloc (sizeof t)
  op (PUSH32 addr)
  storeMultibyte (sizeof t)
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
      storeAddressed (sizeof tyL) addr oldAddr
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
codegenStmt (STimes until block) = do
  -- Assign target value
  op (PUSH32 until)
  op JUMPDEST

  -- Prepare true value of current PC
  op PC
  op (PUSH32 0x01)
  op SWAP1
  op SUB

  -- Decrease target value
  op SWAP1
  op (PUSH32 0x01)
  op SWAP1
  op SUB

  -- Code body
  executeBlock block

  -- Jump to destination back if target value is nonzero
  op DUP1
  op SWAP2
  op JUMPI
  op POP

codegenStmt (SWhile pred block) = do
  Operand predTy predAddr <- codegenExpr pred
  checkTyEq "index_of_while_pred" TBool predTy

  load (sizeof TBool) predAddr
  op DUP1
  op ISZERO -- Reversing this bit because we jump to outside of while initally if predicate is false

  -- Don't enter the loop if predicate is already false
  -- predOffset <- estimateOffsetExpr pred
  -- blockOffset <- estimateOffset block

  -- FIXME: This instruction set depends on implementation of `load` method. Find a way!
  -- let costToWhile = pcCosts [PC, ADD, JUMPI, JUMPDEST, PC, PUSH32 0, SWAP1, SUB, SWAP1, POP, PUSH32 0, PUSH32 0, MLOAD, DIV, SWAP1, DUP1, SWAP2, SWAP1, JUMPI] + blockOffset + predOffset
  rec op (PUSH32 whileOut)
      -- op PC
      -- op ADD
      op JUMPI

      -- Loop start
      op JUMPDEST

      -- Prepare true value of current PC
      op PC
      op (PUSH32 0x01)
      op SWAP1
      op SUB

      -- Code body
      executeBlock block

      -- Load predicate again
      op SWAP1
      op POP
      Operand predTy' predAddr' <- codegenExpr pred
      checkTyEq "index_of_while_pred" TBool predTy'
      load (sizeof TBool) predAddr'
      op SWAP1

      -- Jump to destination back if target value is nonzero
      op DUP1
      op SWAP2
      op SWAP1
      op JUMPI
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

      load (sizeof aTy) addr

      load (sizeof TInt) iAddr
      op (PUSH32 (sizeInt (sizeof aTy)))
      op MUL
      op (PUSH32 oldAddr)
      op ADD

      storeMultibyte (sizeof aTy)
    Def other _ -> throwError $ InternalError $ "codegenStmt ArrAssignment: non-array type is in symbol table as a definition for ArrAssignment code generation: " <> show other

codegenStmt (SVarDecl ty name) =
  declVar ty name

codegenStmt (SDeclAndAssignment ty name val) = do
  codegenStmt (SVarDecl ty name)
  codegenStmt (SAssignment name val)

codegenStmt (SIf ePred bodyBlock) = do
  Operand tyPred addrPred <- codegenExpr ePred
  checkTyEq "if_expr" tyPred TBool

  load (sizeof TBool) addrPred
  op ISZERO -- Negate for jumping condition

  -- offset <- estimateOffset bodyBlock
  rec op (PUSH32 ifOut) -- +3 because of the following `PC`, `ADD` and `JUMPI` instructions.)
      -- op PC
      -- op ADD
      op JUMPI

      void $ executeBlock bodyBlock
      ifOut <- jumpdest
  pure ()

codegenStmt (SIfThenElse ePred trueBlock falseBlock) = do
  Operand tyPred addrPred <- codegenExpr ePred
  checkTyEq "if_else_expr" tyPred TBool

  load (sizeof TBool) addrPred
  op ISZERO -- Negate for jumping condition

  -- trueOffset <- estimateOffset trueBlock
  rec -- let trueJumpDest = pcCosts [PC, ADD, JUMPI, PUSH32 0, PC, ADD, JUMP] + trueOffset
      op (PUSH32 trueDest)
      -- op PC
      -- op ADD
      op JUMPI

      executeBlock trueBlock
      -- falseOffset <- estimateOffset falseBlock

      -- let falseJumpDest = pcCosts [PC, ADD, JUMP, JUMPDEST] + falseOffset
      op (PUSH32 falseDest)
      -- op PC
      -- op ADD
      op JUMP

      trueDest <- jumpdest
      executeBlock falseBlock
      falseDest <- jumpdest
  pure ()

codegenStmt (SReturn retExpr) =
  void (codegenExpr retExpr)

codegenStmt (SExpr expr) = void (codegenExpr expr)

-- estimateOffsetExpr :: (MonadError CodegenError m, MonadState CodegenState m) => Expr -> m Integer
-- estimateOffsetExpr expr =
--   get >>= eitherToError . go
--     where
--       go :: CodegenState -> Either CodegenError Integer
--       go state =
--         let oldPc = _pc state
--             result = execStateT (runEvm (codegenExpr expr)) state
--         in
--           case result of
--             Left err -> Left err
--             Right newState -> Right (_pc newState - oldPc)

-- estimateOffset :: (MonadError CodegenError m, MonadState CodegenState m) => Block -> m Integer
-- estimateOffset block =
--   get >>= eitherToError . go block
--     where
--       go :: Block -> CodegenState -> Either CodegenError Integer
--       go (Block []) _ = Right 0
--       go (Block (stmt:xs)) state =
--         let oldPc = _pc state
--             result = execStateT (runEvm (codegenStmt stmt)) state
--         in
--           case result of
--             Left err -> Left err
--             Right newState -> do
--               let newPc = _pc newState
--               let diff = newPc - oldPc
--               (+ diff) <$> go (Block xs) newState

-- | This type alias will be used for top-level codegen, since
-- at top level we use all contexts
type CodegenM m = (OpcodeM m, MonadState CodegenState m, MemoryM m, MonadError CodegenError m, ContextM m, TcM m, MonadFix m)

data TOperand a = TOperand Addr

tbinop :: CodegenM m => Instruction -> PrimType -> TOperand a -> TOperand a -> m (TOperand a)
tbinop instr ty (TOperand addrl) (TOperand addrr) = do
  load (sizeof ty) addrl
  load (sizeof ty) addrr
  op instr
  addr <- alloc (sizeof ty)
  op (PUSH32 addr)
  storeMultibyte (sizeof ty)
  return (TOperand addr)

type BinOp a = forall m. CodegenM m => TOperand a -> TOperand a -> m (TOperand a)

(+:) :: BinOp Int
(+:) = tbinop ADD TInt

(-:) :: BinOp Int
(-:) = tbinop SUB TInt

(/:) :: BinOp Int
(/:) = tbinop DIV TInt

registerFunction :: forall m. (MonadError CodegenError m, ContextM m, TcM m, MemoryM m, MonadState CodegenState m) => Name -> [(PrimType, Name)] -> m ()
registerFunction name args = do
  allocatedArgs <- mapM allocArg args
  funcRegistry %= M.insert name allocatedArgs
    where
      allocArg :: (PrimType, Name) -> m (PrimType, Name, Integer)
      allocArg (ty, argName) = do
        addr <- alloc (sizeof ty)
        declVar ty argName
        assign ty argName addr
        return (ty, argName, addr)

codegenFunDef :: CodegenM m => FunStmt -> m ()
codegenFunDef (FunStmt name args block retTyAnnot) = do
  registerFunction name args
  -- offset <- estimateOffset block
  -- op $ PUSH32 $ pcCosts [PC, ADD, JUMP, JUMPDEST, JUMP] + offset
  rec op $ PUSH32 functionOut
      -- op PC
      -- op ADD
      op JUMP
      funPc <- use pc
      op JUMPDEST
      Operand retTy retAddr <- executeFunBlock block
      checkTyEq "function definition" retTyAnnot retTy
      op JUMP -- Before calling function, we push PC, so we remember and jump to it
      functionOut <- jumpdest
  updateCtx (M.insert name (TFun retTy, FunAddr funPc retAddr))
  return ()
    where
      executeFunBlock :: forall m. CodegenM m => Block -> m Operand
      executeFunBlock (Block stmts) = do
        go stmts
          where
            go :: [Stmt] -> m Operand
            go []             = throwError NoReturnStatement
            go [SReturn expr] = codegenExpr expr
            go (stmt:xs)      = codegenStmt stmt >> go xs

takeArgsToContext :: forall m. CodegenM m => String -> [(PrimType, String, Integer)] -> [Operand] -> m ()
takeArgsToContext funcName registryArgs callerArgs = do
  unless (length registryArgs == length callerArgs) $
    throwError (FuncArgLengthMismatch funcName (length registryArgs) (length callerArgs))
  mapM_ bindArg (zip registryArgs callerArgs)
    where
      bindArg :: ((PrimType, String, Integer), Operand) -> m ()
      bindArg ((registryTy, registryName, registryAddr), Operand callerTy callerAddr) = do
        checkTyEq registryName registryTy callerTy
        storeAddressed (sizeof callerTy) callerAddr registryAddr

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
      -- op (PUSH32 (pcCosts [PUSH32 0, JUMP, PUSH32 0, JUMP, JUMPDEST]))
      rec op (PUSH32 funcDest)

          -- Jumping to function
          op (PUSH32 funAddr)
          op JUMP
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
  storeVal (sizeof TInt) val addr
  return (Operand TInt addr)

codegenExpr (EChar val) = do
  addr <- alloc (sizeof TChar)
  storeVal (sizeof TChar) (fromIntegral (ord val)) addr
  return (Operand TChar addr)

codegenExpr (EBool val) = do
  addr <- alloc (sizeof TBool)
  storeVal (sizeof TBool) (boolToInt val) addr
  return (Operand TBool addr)

codegenExpr (EBinop binop expr1 expr2) = do
  Operand ty1 left <- codegenExpr expr1
  Operand ty2 right <- codegenExpr expr2
  case (ty1, ty2) of
    (TInt, TInt) ->
      case binop of
        OpAdd -> binOp TInt ADD left right
        OpMul -> binOp TInt MUL left right
        OpSub -> binOp TInt SUB left right
        OpDiv -> binOp TInt DIV left right
        OpMod -> binOp TInt MOD left right
        OpGt  -> binOp TBool GT left right
        OpLt  -> binOp TBool LT left right
        OpEq  -> binOp TBool EQ left right
    _ -> throwError $ WrongOperandTypes ty1 ty2

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
