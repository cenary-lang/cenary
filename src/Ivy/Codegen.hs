{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Lens                   hiding (op, assign)
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Logger.CallStack (logInfo, logDebug, logWarn)
import           Control.Monad.State
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

-- useSymTable :: ScopeLevel -> Evm SymbolTable
-- useSymTable level = do
--   touchLevel level
--   tables <- use symTables
--   case M.lookup level tables of
--     Nothing ->
--       M.empty <$ logWarn "Something is not right. Check here."
--     Just table ->
--       return table

-- touchLevel :: ScopeLevel -> Evm ()
-- touchLevel level = do
--   tables <- use symTables
--   case M.lookup level tables of
--     Just t  -> return ()
--     Nothing -> symTables %= M.insert level M.empty

-- updateSymTable :: ScopeLevel -> (SymbolTable -> SymbolTable) -> Evm ()
-- updateSymTable level f = do
--   touchLevel level
--   symTables %= M.update (Just . f) level

initScopeLevel :: Int
initScopeLevel = 0

executeBlock :: Block -> Evm ()
executeBlock (Block bodyExpr) = do
  g <- use globalScope
  l <- use localScope
  globalScope .= l <> g
  localScope .= M.empty
  mapM_ codegenTop' bodyExpr
  globalScope .= g -- Reset global scope
  localScope .= l -- Reset local scope

assign :: Type -> Name -> Integer -> Evm ()
assign ty name addr = do
  g <- use globalScope
  l <- use localScope
  case M.lookup name g of
    Just _addr' ->
      globalScope %= M.update (const (Just (ty, Just addr))) name
    Nothing ->
      case M.lookup name l of
        Just _addr' -> localScope %= M.update (const (Just (ty, Just addr))) name
        Nothing -> throwError (VariableNotDeclared name)

decl :: Type -> Name -> Evm ()
decl ty name = do
  l <- use localScope
  case M.lookup name l of
    Nothing -> localScope %= M.insert name (ty, Nothing)
    Just _ -> throwError (VariableAlreadyDeclared name)

lookup :: String -> Evm VariableStatus
lookup name = do
  g <- M.lookup name <$> use globalScope
  l <- M.lookup name <$> use localScope
  decide g l
  where
    -- FIXME
    decide :: Maybe (Type, Maybe Integer) -> Maybe (Type, Maybe Integer) -> Evm VariableStatus
    decide Nothing           Nothing           = return NotDeclared
    decide Nothing           (Just (ty, Nothing))    = return $ Decl ty Local
    decide Nothing           (Just (ty, Just val)) = return $ Def ty Local val
    decide (Just (ty, Nothing))    Nothing           = return $ Decl ty Global
    decide (Just (_ty1, Nothing))    (Just (_ty2, Nothing)) = throwError (VariableAlreadyDeclared name)
    decide (Just (ty1, Nothing))    (Just (ty2, Just val)) =
      if ty1 == ty2
         then return $ Def ty1 Local val
         else throwError (ScopedTypeViolation name ty1 ty2)
    decide (Just (ty, Just val)) Nothing           = return $ Def ty Global val
    decide (Just (_ty1, Just val)) (Just (_ty2, Nothing)) = throwError (VariableAlreadyDeclared name)
    decide (Just (ty1, Just _))   (Just (ty2, Just val)) =
      if ty1 == ty2
         then return $ Def ty1 Local val -- Local value overrides
         else throwError (ScopedTypeViolation name ty1 ty2)

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
    Decl tyL _ -> do
      unless (tyL == tyR) $ throwError $ TypeMismatch name tyL tyR
      op2 PUSH32 addr
      op MLOAD
      newAddr <- alloc
      op2 PUSH32 newAddr
      op MSTORE
      assign tyL name newAddr
      -- updateSymTable level $ M.update (const (Just (Just newAddr))) name
      return Nothing
    Def tyL _ oldAddr -> do
      unless (tyL == tyR) $ throwError $ TypeMismatch name tyL tyR
      op2 PUSH32 addr
      op MLOAD
      op2 PUSH32 oldAddr
      op MSTORE
      assign tyL name oldAddr
      -- updateSymTable level $ M.update (const (Just (Just oldAddr))) name
      return Nothing

codegenTop (VarDecl ty name) =
  Nothing <$ decl ty name

codegenTop (Identifier name) = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name)
    Decl _ _ -> throwError (VariableNotDefined name)
    Def ty _ addr -> return (Just (Operand ty addr))

codegenTop (PrimInt val) = do
  addr <- alloc
  op2 PUSH32 val
  op2 PUSH32 addr
  op MSTORE
  return (Just (Operand IntT addr))

codegenTop (BinaryOp op expr1 expr2) = do
  Operand _ left <- codegenTopOperand expr1
  Operand _ right <- codegenTopOperand expr2
  case op of
    OpAdd -> binOp IntT ADD left right
    OpMul -> binOp IntT MUL left right
    OpSub -> binOp IntT SUB left right
    OpDiv -> binOp IntT DIV left right

-- checkTyEq :: Type -> Type -> Evm Type
-- checkTyEq ty1 ty2 =
--   case (ty1, ty2) of
--     (IntT, IntT) -> return IntT
--     _            -> throwError $ TypeMismatch $ "Expected " <> T.pack (show ty1) <> " and " <> T.pack (show ty2) <> "to have equal types"

getEnv :: Evm SymbolTable
getEnv = liftA2 (<>) (use localScope) (use globalScope)

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
  use byteCode >>= log "ByteCode"
  use localScope >>= log "LocalScope"
  use globalScope >>= log "GlobalScope"
  codegenTop expr
