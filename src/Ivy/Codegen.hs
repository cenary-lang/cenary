{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens                   hiding (Context, assign, op)
import           Control.Monad.Except
import           Control.Monad.Logger           hiding (logDebug, logInfo)
import           Control.Monad.Logger.CallStack (logDebug, logInfo, logWarn)
import           Control.Monad.State
import           Data.Char                      (ord)
import           Data.Either.Combinators        (eitherToError)
import           Data.Functor                   (($>))
import qualified Data.Map                       as M
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Prelude                        hiding (log, lookup)
--------------------------------------------------------------------------------
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types
import           Ivy.EvmAPI.Instruction
import           Ivy.Parser
import           Ivy.Syntax
--------------------------------------------------------------------------------

class ContextM m where
  updateCtx :: (Context -> Context) -> m ()
  lookup :: String -> m VariableStatus

instance ContextM Evm where
  updateCtx f =
    env %= (\(ctx:xs) -> (f ctx:xs))
  lookup name =
    go =<< use env
    where
      go :: [Context] -> Evm VariableStatus
      go [] = return NotDeclared
      go (ctx:xs) =
        case M.lookup name ctx of
          Just (TFun retTy, FunAddr funAddr retAddr)   -> return (FunDef retTy funAddr retAddr)
          Just (varTy, VarAddr varAddr) -> decideVar (varTy, varAddr)
          Nothing -> go xs
        where
          decideVar (ty, Nothing)   = return (Decl ty)
          decideVar (ty, Just addr) = return (Def ty addr)

class Monad m => TcM m where
  checkTyEq :: Name -> PrimType -> PrimType -> m ()

instance TcM Evm where
  checkTyEq name tyL tyR =
    unless (tyL == tyR) $ throwError $ TypeMismatch name tyR tyL

binOp :: (OpcodeM m, MemoryM m) => PrimType -> Instruction -> Integer -> Integer -> m Operand
binOp t instr left right = do
  load (sizeof TInt) left
  load (sizeof TInt) right
  op instr
  addr <- alloc (sizeof TInt)
  op2 PUSH32 addr
  storeMultibyte (sizeof TInt)
  return (Operand t addr)

initCodegenState :: CodegenState
initCodegenState = CodegenState
  { _byteCode    = ""
  , _memPointers = initMemPointers
  , _memory      = M.empty
  , _env         = [M.empty]
  , _pc          = 0
  }

executeBlock :: CodegenM m => Block -> m ()
executeBlock (Block stmts) = do
  env %= (M.empty :)
  mapM_ codegenTop' stmts
  env %= tail

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

codegenTop :: CodegenM m => Stmt -> m ()
codegenTop (STimes until block) = do
  -- Assign target value
  op2 PUSH32 until
  op JUMPDEST

  -- Prepare true value of current PC
  op PC
  op2 PUSH32 0x01
  op SWAP1
  op SUB

  -- Decrease target value
  op SWAP1
  op2 PUSH32 0x01
  op SWAP1
  op SUB

  -- Code body
  executeBlock block
  -- Jump to destination back if target value is nonzero
  op DUP1
  op SWAP2
  op JUMPI

codegenTop (SAssignment name val) = do
  Operand tyR addr <- codegenExpr val
  assign tyR name addr

codegenTop stmt@(SArrAssignment name index val) = do
  Operand tyI iAddr <- codegenExpr index
  checkTyEq ("index_of_" <> name) TInt tyI
  Operand tyR addr <- codegenExpr val
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name (StmtDetails stmt)
    Decl _tyL -> throwError $ InternalError "codegenTop ArrAssignment: array type variable is in Def state"
    Def (TArray length aTy) oldAddr -> do
      checkTyEq name aTy tyR

      load (sizeof aTy) addr

      load (sizeof TInt) iAddr
      op2 PUSH32 (sizeInt (sizeof aTy))
      op MUL
      op2 PUSH32 oldAddr
      op ADD

      storeMultibyte (sizeof aTy)
    Def other _ -> throwError $ InternalError "codegenTop ArrAssignment: non-array type is in symbol table as a definition for ArrAssignment code generation"

codegenTop (SVarDecl ty name) =
  lookup name >>= \case
    Decl _ -> throwError (VariableAlreadyDeclared name)
    Def _ _ -> throwError (VariableAlreadyDeclared name)
    NotDeclared -> do
      mb_addr <- case ty of
          TArray length aTy -> Just <$> allocBulk length (sizeof aTy)
          _                 -> return Nothing
      updateCtx (M.insert name (ty, VarAddr mb_addr))

codegenTop (SDeclAndAssignment ty name val) = do
  codegenTop (SVarDecl ty name)
  codegenTop (SAssignment name val)

codegenTop (SIf ePred bodyBlock) = do
  Operand tyPred addrPred <- codegenExpr ePred
  checkTyEq "if_expr" tyPred TBool

  load (sizeof TBool) addrPred
  op ISZERO -- Negate for jumping condition

  offset <- estimateOffset bodyBlock
  op2 PUSH32 (offset + 3) -- +3 because of the following `PC`, `ADD` and `JUMPI` instructions.
  op PC
  op ADD
  op JUMPI

  void $ executeBlock bodyBlock
  op JUMPDEST

codegenTop (SIfThenElse ePred trueBlock falseBlock) = do
  Operand tyPred addrPred <- codegenExpr ePred
  checkTyEq "if_else_expr" tyPred TBool

  load (sizeof TBool) addrPred
  op ISZERO -- Negate for jumping condition

  trueOffset <- estimateOffset trueBlock
  let trueJumpDest = pcCost PC + pcCost ADD + pcCost JUMPI + trueOffset + pcCost PUSH32 + pcCost PC + pcCost ADD + pcCost JUMP

  op2 PUSH32 trueJumpDest
  op PC
  op ADD
  op JUMPI

  executeBlock trueBlock
  falseOffset <- estimateOffset falseBlock

  let falseJumpDest = pcCosts [PC, ADD, JUMP, JUMPDEST] + falseOffset
  op2 PUSH32 falseJumpDest
  op PC
  op ADD
  op JUMP

  op JUMPDEST
  executeBlock falseBlock
  op JUMPDEST

codegenTop (SReturn retExpr) =
  void (codegenExpr retExpr)

codegenTop (SFunDef name block@(Block body) retTyAnnot) = do
  offset <- estimateOffset block
  op2 PUSH32 $ pcCosts [PC, ADD, JUMP, JUMPDEST, JUMP] + offset
  op PC
  op ADD
  op JUMP
  funPc <- use pc
  op JUMPDEST
  Operand retTy retAddr <- executeFunBlock block
  checkTyEq "function definition" retTyAnnot retTy
  op JUMP -- Before calling function, we push PC, so we remember and jump to it
  op JUMPDEST
  updateCtx (M.insert name (TFun retTy, FunAddr funPc retAddr))
  return ()
    where
      executeFunBlock :: forall m. CodegenM m => Block -> m Operand
      executeFunBlock (Block stmts) = do
        env %= (M.empty :)
        go stmts <* (env %= tail)
          where
            go :: [Stmt] -> m Operand
            go []             = throwError NoReturnStatement
            go [SReturn expr] = codegenExpr expr
            go (stmt:xs)      = codegenTop' stmt >> go xs

codegenTop (SExpr expr) = void (codegenExpr expr)

estimateOffset :: (MonadError CodegenError m, MonadState CodegenState m) => Block -> m Integer
estimateOffset block = do
  get >>= eitherToError . go block
    where
      go :: Block -> CodegenState -> (Either CodegenError Integer)
      go (Block []) state = Right 0
      go (Block (stmt:xs)) state = do
        let oldPc = _pc state
        let result = execStateT (runEvm (codegenTop stmt)) state
        case result of
          Left err -> Left err
          Right newState -> do
            let newPc = _pc newState
            let diff = newPc - oldPc
            ((+ diff) <$> ) (go (Block xs) newState)

type CodegenM m = (OpcodeM m, MonadState CodegenState m, MemoryM m, MonadError CodegenError m, ContextM m, TcM m)
codegenExpr :: CodegenM m => Expr -> m Operand
codegenExpr expr@(EIdentifier name) = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (ExprDetails expr))
    Decl _ -> throwError (VariableNotDefined name)
    Def ty addr -> return (Operand ty addr)

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

codegenExpr (EBinop op expr1 expr2) = do
  Operand ty1 left <- codegenExpr expr1
  Operand ty2 right <- codegenExpr expr2
  case (ty1, ty2) of
    (TInt, TInt) ->
      case op of
        OpAdd -> binOp TInt ADD left right
        OpMul -> binOp TInt MUL left right
        OpSub -> binOp TInt SUB left right
        OpDiv -> binOp TInt DIV left right
    _ -> throwError $ WrongOperandTypes ty1 ty2

codegenExpr expr@(EFunCall name) =
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name (ExprDetails expr))
    FunDef retTy funAddr retAddr -> do
      -- Preparing checkpoint
      op PC
      pc' <- use pc
      op2 PUSH32 (pcCosts [PUSH32, JUMP, PUSH32, JUMP, JUMPDEST])
      op ADD

      -- Jumping to function
      op2 PUSH32 (pc' + pcCosts [PC, PUSH32, JUMP])
      op JUMP
      op JUMPDEST

      return (Operand retTy retAddr)
    _ -> throwError $ InternalError "Function call's name lookup is neither NotDeclared nor FunDef."

codegenTop' :: CodegenM m => Stmt -> m ()
codegenTop' stmt = do
  codegenTop stmt
