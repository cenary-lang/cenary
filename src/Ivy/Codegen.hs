{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--------------------------------------------------------------------------------
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types
import           Ivy.EvmAPI.Instruction
import           Ivy.Parser
import           Ivy.Syntax
--------------------------------------------------------------------------------

binOp :: PrimType -> Instruction -> Integer -> Integer -> Evm (Maybe Operand)
binOp t instr left right = do
  load (sizeof TInt) left
  load (sizeof TInt) right
  op instr
  addr <- alloc (sizeof TInt)
  op2 PUSH32 addr
  storeMultibyte (sizeof TInt)
  return (Just (Operand t addr))

initCodegenState :: CodegenState
initCodegenState = CodegenState
  { _byteCode    = ""
  , _memPointers = initMemPointers
  , _memory      = M.empty
  , _env         = [M.empty]
  , _pc          = 0
  }

executeBlock :: Block -> Evm ()
executeBlock (Block bodyExpr) = do
  env %= (M.empty :)
  mapM_ codegenTop' bodyExpr
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
lookup name = do
  go =<< use env
  -- g <- M.lookup name <$> use globalScope
  -- l <- M.lookup name <$> use localScope
  -- decide g l
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

codegenTop :: Expr -> Evm (Maybe Operand)
codegenTop (ETimes until block) = do
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

  return Nothing

codegenTop (EAssignment name val) = do
  Operand tyR addr <- codegenTopOperand val
  assign tyR name addr
  return Nothing

codegenTop (EArrAssignment name index val) = do
  Operand tyI iAddr <- codegenTopOperand index
  checkTyEq ("index_of_" <> name) TInt tyI
  Operand tyR addr <- codegenTopOperand val
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
      return Nothing
    Def other _ -> throwError $ InternalError "codegenTop ArrAssignment: non-array type is in symbol table as a definition for ArrAssignment code generation"

codegenTop (EVarDecl ty name) = do
  lookup name >>= \case
    Decl _ -> throwError (VariableAlreadyDeclared name)
    Def _ _ -> throwError (VariableAlreadyDeclared name)
    NotDeclared -> do
      mb_addr <- case ty of
          TArray length aTy -> Just <$> allocBulk length (sizeof aTy)
          _                 -> return Nothing
      updateCtx (M.insert name (ty, mb_addr))
  return Nothing

codegenTop (EDeclAndAssignment ty name val) = do
  codegenTop (EVarDecl ty name)
  codegenTop (EAssignment name val)
  return Nothing

codegenTop (EIdentifier name) = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name)
    Decl _ -> throwError (VariableNotDefined name)
    Def ty addr -> return (Just (Operand ty addr))

codegenTop (EInt val) = do
  addr <- alloc (sizeof TInt)
  storeVal (sizeof TInt) val addr
  return (Just (Operand TInt addr))

codegenTop (EChar val) = do
  addr <- alloc (sizeof TChar)
  storeVal (sizeof TChar) (fromIntegral (ord val)) addr
  return (Just (Operand TChar addr))

codegenTop (EBool val) = do
  addr <- alloc (sizeof TBool)
  storeVal (sizeof TBool) (boolToInt val) addr
  return (Just (Operand TBool addr))

codegenTop (EBinop op expr1 expr2) = do
  Operand ty1 left <- codegenTopOperand expr1
  Operand ty2 right <- codegenTopOperand expr2
  case (ty1, ty2) of
    (TInt, TInt) ->
      case op of
        OpAdd -> binOp TInt ADD left right
        OpMul -> binOp TInt MUL left right
        OpSub -> binOp TInt SUB left right
        OpDiv -> binOp TInt DIV left right
    _ -> throwError $ WrongOperandTypes ty1 ty2

codegenTop (EDebug expr) = do
  Operand _ty addr <- codegenTopOperand expr
  op2 PUSH32 addr
  op MLOAD
  op2 PUSH32 0x05
  op2 PUSH32 0x03
  op LOG1
  return Nothing

codegenTop (EIf ePred bodyBlock) = do
  Operand tyPred addrPred <- codegenTopOperand ePred
  checkTyEq "if_expr" tyPred TBool

  load (sizeof TBool) addrPred
  op ISZERO -- Because we'll jump if it's FALSE

  offset' <- get >>= liftIO . estimateOffset bodyBlock
  case offset' of
    Left err -> throwError err
    Right offset -> do
      op2 PUSH32 (offset + 3) -- +3 because of the following `PC`, `ADD` and `JUMPI` instructions.
      op PC
      op ADD
      op JUMPI

      void $ executeBlock bodyBlock
      op JUMPDEST
      return Nothing

estimateOffset :: Block -> CodegenState -> IO (Either CodegenError Integer)
estimateOffset (Block []) state = return (Right 0)
estimateOffset (Block (expr:xs)) state = do
  let oldPc = _pc state
  result <- liftIO $ runExceptT (runStdoutLoggingT (execStateT (runEvm (codegenTop expr)) state))
  case result of
    Left err -> return (Left err)
    Right newState -> do
      let newPc = _pc newState
      let diff = newPc - oldPc
      fmap (fmap (+ diff)) $ estimateOffset (Block xs) newState

markDest :: Evm ()
markDest = do
  op JUMPDEST
  op PC
  op2 PUSH32 0x01
  op SWAP1
  op SUB

codegenTopOperand :: Expr -> Evm Operand
codegenTopOperand expr =
  codegenTop' expr >>= \case
    Just val -> return val
    Nothing -> throwError $ InternalError $ "Following expression should have returned an operand, but it didn't: " <> show expr

log :: Show a => T.Text -> a -> Evm ()
log desc k = logDebug $ "[" <> desc <> "]: " <> T.pack (show k)

codegenTop' :: Expr -> Evm (Maybe Operand)
codegenTop' expr = do
  log "Expr" expr
  -- use byteCode >>= log "ByteCode"
  use env >>= log "env"
  -- use memory >>= log "Memory"
  -- use memPointers >>= log "MemPointers"
  codegenTop expr
