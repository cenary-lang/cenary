{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Lens                   hiding (op, assign, Context)
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Logger.CallStack (logInfo, logDebug, logWarn)
import           Control.Monad.State
import           Data.Char                      (ord)
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import           Data.Monoid                    ((<>))
import           Prelude hiding                 (lookup, log)
--------------------------------------------------------------------------------
import           Ivy.EvmAPI.Instruction
import           Ivy.Codegen.Types
import           Ivy.Parser
import           Ivy.Syntax
--------------------------------------------------------------------------------

initScopeLevel :: Int
initScopeLevel = 0

executeBlock :: Block -> Evm ()
executeBlock (Block bodyExpr) = do
  env %= (M.empty :)
  mapM_ codegenTop' bodyExpr
  env %= tail

updateCtx :: (Context -> Context) -> Evm ()
updateCtx f =
  env %= (\(ctx:xs) -> (f ctx:xs))

assign :: PrimType -> Name -> Integer -> Evm ()
assign ty name addr = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name)
    Decl ty2 -> do
      checkTyEq name ty ty2
      updateCtx (M.update (const (Just (ty, Just addr))) name)
    Def ty2 oldAddr -> do
      checkTyEq name ty ty2
      updateCtx (M.update (const (Just (ty, Just addr))) name)

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
        Just (ty, Nothing) -> return (Decl ty)
        Just (ty, Just addr) -> return (Def ty addr)
        Nothing -> go xs

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
codegenTop (Times until block) = do
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

codegenTop (Assignment name val) = do
  Operand tyR addr <- codegenTopOperand val
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name
    Decl tyL -> do
      checkTyEq name tyL tyR
      -- newAddr <- alloc (sizeof tyR)
      -- storeAddressed (sizeof tyR) addr newAddr
      assign tyL name addr
      return Nothing
    Def tyL oldAddr -> do
      checkTyEq name tyL tyR
      -- storeAddressed (sizeof tyL) addr oldAddr
      assign tyL name addr
      return Nothing

codegenTop (ArrAssignment name index val) = do
  Operand tyR addr <- codegenTopOperand val
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name
    Decl _tyL -> throwError $ InternalError "codegenTop ArrAssignment: array type variable is in Def state"
    Def (Array _size aTy) oldAddr -> do
      checkTyEq name aTy tyR
      op2 PUSH32 (addr + sizeInt (sizeof aTy) * index)
      op MLOAD
      op2 PUSH32 oldAddr
      op MSTORE
      -- assign tyL name oldAddr
      return Nothing
    Def other _ -> throwError $ InternalError "codegenTop ArrAssignment: non-array type is in symbol table as a definition for ArrAssignment code generation"

codegenTop (VarDecl ty name) = do
  lookup name >>= \case
    Decl _ -> throwError (VariableAlreadyDeclared name)
    Def _ _ -> throwError (VariableAlreadyDeclared name)
    NotDeclared -> do
      mb_addr <- case ty of
          Array length aTy -> Just <$> allocBulk length (sizeof aTy)
          _              -> return Nothing
      updateCtx (M.insert name (ty, mb_addr))
  return Nothing

codegenTop (Identifier name) = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name)
    Decl _ -> throwError (VariableNotDefined name)
    Def ty addr -> return (Just (Operand ty addr))

codegenTop (IntExpr val) = do
  addr <- alloc (sizeof IntT)
  storeVal (sizeof IntT) val addr
  return (Just (Operand IntT addr))

codegenTop (CharExpr val) = do
  addr <- alloc (sizeof CharT)
  storeVal (sizeof CharT) (fromIntegral (ord val)) addr
  return (Just (Operand CharT addr))

codegenTop (BinaryOp op expr1 expr2) = do
  Operand ty1 left <- codegenTopOperand expr1
  Operand ty2 right <- codegenTopOperand expr2
  case (ty1, ty2) of
    (IntT, IntT) ->
      case op of
        OpAdd -> binOp IntT ADD left right
        OpMul -> binOp IntT MUL left right
        OpSub -> binOp IntT SUB left right
        OpDiv -> binOp IntT DIV left right
    _ -> throwError $ WrongOperandTypes ty1 ty2

codegenTop (Debug expr) = do
  Operand _ty addr <- codegenTopOperand expr
  op2 PUSH32 addr
  op MLOAD
  op2 PUSH32 0x05
  op2 PUSH32 0x03
  op LOG1
  return Nothing

codegenTopOperand :: Expr -> Evm Operand
codegenTopOperand expr =
  codegenTop' expr >>= \case
    Just val -> return val
    Nothing -> throwError $ InternalError $ "Following expression should have returned an operand, but it didn't: " <> show expr

log :: Show a => T.Text -> a -> Evm ()
log desc k = logDebug $ "[" <> desc <> "]: " <> T.pack (show k)

codegenTop' :: Expr -> Evm (Maybe Operand)
codegenTop' expr =
  -- log "Expr" expr
  -- use byteCode >>= log "ByteCode"
  -- use localScope >>= log "LocalScope"
  -- use globalScope >>= log "GlobalScope"
  -- use memory >>= log "Memory"
  -- use memPointers >>= log "MemPointers"
  codegenTop expr
