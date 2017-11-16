{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens                   hiding (Context, assign, op)
import           Control.Monad.Except
import           Control.Monad.Logger           hiding (logDebug, logInfo)
import           Control.Monad.Logger.CallStack (logDebug, logInfo, logWarn)
import           Control.Monad.State
import           Data.Char                      (ord)
import           Data.Functor                   (($>))
import qualified Data.Map                       as M
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Prelude                        hiding (log, lookup)
import           Data.Either.Combinators        (eitherToError)
--------------------------------------------------------------------------------
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types
import           Ivy.EvmAPI.Instruction
import           Ivy.Parser
import           Ivy.Syntax
--------------------------------------------------------------------------------

binOp :: PrimType -> Instruction -> Integer -> Integer -> Evm Operand
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

executeBlock :: Block -> Evm ()
executeBlock (Block stmts) = do
  env %= (M.empty :)
  mapM_ codegenTop' stmts
  env %= tail

updateCtx :: (Context -> Context) -> Evm ()
updateCtx f =
  env %= (\(ctx:xs) -> (f ctx:xs))

assign :: PrimType -> Name -> Integer -> Evm ()
assign tyR name addr = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name)
    Decl tyL -> do
      checkTyEq name tyL tyR
      updateCtx (M.update (const (Just (tyL, Just addr))) name)
    Def tyL oldAddr -> do
      checkTyEq name tyL tyR
      storeAddressed (sizeof tyL) addr oldAddr
      updateCtx (M.update (const (Just (tyL, Just addr))) name)

lookup :: String -> Evm VariableStatus
lookup name =
  go =<< use env
  where
    go :: [Context] -> Evm VariableStatus
    go [] = return NotDeclared
    go (ctx:xs) =
      case M.lookup name ctx of
        Just (ty, Nothing)   -> return (Decl ty)
        Just (ty, Just addr) -> return (Def ty addr)
        Nothing              -> go xs

    decide :: Maybe (PrimType, Maybe Integer) -> Maybe (PrimType, Maybe Integer) -> Evm VariableStatus
    decide Nothing                   Nothing                = return NotDeclared
    decide Nothing                   (Just (ty, Nothing))   = return $ Decl ty
    decide Nothing                   (Just (ty, Just val))  = return $ Def ty val
    decide (Just (ty, Nothing))      Nothing                = return $ Decl ty
    decide (Just (_ty1, Nothing))    (Just (_ty2, Nothing)) = throwError (VariableAlreadyDeclared name)
    decide (Just (ty1, Nothing))     (Just (ty2, Just val)) =
      if ty1 == ty2
         then return $ Def ty1 val
         else throwError (ScopedTypeViolation name ty1 ty2)
    decide (Just (ty, Just val))      Nothing                = return $ Def ty val
    decide (Just (_ty1, Just val))    (Just (_ty2, Nothing)) = throwError (VariableAlreadyDeclared name)
    decide (Just (ty1, Just _))       (Just (ty2, Just val)) =
      if ty1 == ty2
         then return $ Def ty1 val -- Local value overrides
         else throwError (ScopedTypeViolation name ty1 ty2)

checkTyEq :: Name -> PrimType -> PrimType -> Evm ()
checkTyEq name tyL tyR =
  unless (tyL == tyR) $ throwError $ TypeMismatch name tyR tyL

codegenTop :: Stmt -> Evm ()
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

codegenTop (SArrAssignment name index val) = do
  Operand tyI iAddr <- codegenExpr index
  checkTyEq ("index_of_" <> name) TInt tyI
  Operand tyR addr <- codegenExpr val
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name
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
      updateCtx (M.insert name (ty, mb_addr))

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

  let falseJumpDest = pcCost PC + pcCost ADD + pcCost JUMP + pcCost JUMPDEST + falseOffset
  op2 PUSH32 falseJumpDest
  op PC
  op ADD
  op JUMP

  op JUMPDEST
  executeBlock falseBlock
  op JUMPDEST

codegenTop (SFunDef name block@(Block body) retType) = do
  retOp <- checkRetExistence retType body
  return ()
    where
      checkRetExistence :: PrimType -> [Stmt] -> Evm Operand
      checkRetExistence retTy (reverse -> (expr:_)) =
        case expr of
          SReturn retExpr -> do
            retOp@(Operand myRetTy _) <- codegenExpr retExpr
            retOp <$ checkTyEq "ret_type" myRetTy retTy
          _ -> throwError NoReturnStatement

codegenTop (SFunCall name) = undefined

estimateOffset :: Block -> Evm Integer
estimateOffset block =
  get >>= liftIO . go block >>= eitherToError 
    where
      go :: Block -> CodegenState -> IO (Either CodegenError Integer)
      go (Block []) state = return (Right 0)
      go (Block (stmt:xs)) state = do
        let oldPc = _pc state
        result <- liftIO $ runExceptT (runStdoutLoggingT (execStateT (runEvm (codegenTop stmt)) state))
        case result of
          Left err -> return (Left err)
          Right newState -> do
            let newPc = _pc newState
            let diff = newPc - oldPc
            ((+ diff) <$> ) <$> go (Block xs) newState

codegenExpr :: Expr -> Evm Operand
codegenExpr (EIdentifier name) = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name)
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

log :: Show a => T.Text -> a -> Evm ()
log desc k = logDebug $ "[" <> desc <> "]: " <> T.pack (show k)

codegenTop' :: Stmt -> Evm ()
codegenTop' stmt = do
  use env >>= log "env"
  codegenTop stmt
