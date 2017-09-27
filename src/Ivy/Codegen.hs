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

assign :: Name -> Integer -> Evm ()
assign name addr = do
  g <- use globalScope
  l <- use localScope
  case M.lookup name g of
    Just _addr' ->
      globalScope %= M.update (const (Just (Just addr))) name
    Nothing ->
      case M.lookup name l of
        Just _addr' -> localScope %= M.update (const (Just (Just addr))) name
        Nothing -> throwError (VariableNotDeclared name)

decl :: Name -> Evm ()
decl name = do
  l <- use localScope
  case M.lookup name l of
    Nothing -> localScope %= M.insert name Nothing
    Just _ -> throwError (VariableAlreadyDeclared name)

lookup :: String -> Evm VariableStatus
lookup name = do
  g <- M.lookup name <$> use globalScope
  l <- M.lookup name <$> use localScope
  decide g l
  where
    decide :: Maybe (Maybe Integer) -> Maybe (Maybe Integer) -> Evm VariableStatus
    decide Nothing           Nothing           = return NotDeclared
    decide Nothing           (Just Nothing)    = return $ Decl Local
    decide Nothing           (Just (Just val)) = return $ Def Local val
    decide (Just Nothing)    Nothing           = return $ Decl Global
    decide (Just Nothing)    (Just Nothing)    = throwError (VariableAlreadyDeclared name)
    decide (Just Nothing)    (Just (Just val)) = return $ Def Local val
    decide (Just (Just val)) Nothing           = return $ Def Global val
    decide (Just (Just val)) (Just Nothing)    = throwError (VariableAlreadyDeclared name)
    decide (Just (Just _))   (Just (Just val)) = return $ Def Local val -- Local value overrides

codegenTop :: Expr -> Evm (Maybe Integer)
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

-- noglobal ->
--   nolocal ->
--     NotDeclared
--   local without value ->
--     LocalDecl
--   local with value ->
--     LocalDef value
-- global without value ->
--   nolocal ->
--     GlobalDecl
--   local without value ->
--     Error
--   local with value ->
--     LocalDef value
-- global with value ->
--   nolocal ->
--     GlobalDef value
--   local without value ->
--     Error
--   local with value ->
--     LocalDef value

codegenTop (Assignment name val) = do
  addr <- codegenTopOperand val
  lookup name >>= \case
    NotDeclared -> throwError $ VariableNotDeclared name
    Decl _ -> do
      op2 PUSH32 addr
      op MLOAD
      newAddr <- alloc
      op2 PUSH32 newAddr
      op MSTORE
      assign name newAddr
      -- updateSymTable level $ M.update (const (Just (Just newAddr))) name
      return Nothing
    Def _ oldAddr -> do
      op2 PUSH32 addr
      op MLOAD
      op2 PUSH32 oldAddr
      op MSTORE
      assign name oldAddr
      -- updateSymTable level $ M.update (const (Just (Just oldAddr))) name
      return Nothing

codegenTop (VarDecl name) =
  Nothing <$ decl name

codegenTop (Identifier name) = do
  lookup name >>= \case
    NotDeclared -> throwError (VariableNotDeclared name)
    Decl _ -> throwError (VariableNotDefined name)
    Def _ addr -> return (Just addr)

codegenTop (PrimInt val) = do
  addr <- alloc
  op2 PUSH32 val
  op2 PUSH32 addr
  op MSTORE
  return (Just addr)

codegenTop (BinaryOp op expr1 expr2) = do
  left <- codegenTopOperand expr1
  right <- codegenTopOperand expr2
  case op of
    OpAdd -> binOp ADD left right
    OpMul -> binOp MUL left right
    OpSub -> binOp SUB left right
    OpDiv -> binOp DIV left right

getEnv :: Evm SymbolTable
getEnv = liftA2 (<>) (use localScope) (use globalScope)

codegenTopOperand :: Expr -> Evm Integer
codegenTopOperand expr =
  codegenTop' expr >>= \case
    Just val -> return val
    Nothing -> throwError $ InternalError $ "Following expression should have returned an operand, but it didn't: " <> show expr

log :: Show a => T.Text -> a -> Evm ()
log desc k = logDebug $ "[" <> desc <> "]: " <> T.pack (show k)

codegenTop' :: Expr -> Evm (Maybe Integer)
codegenTop' expr = do
  log "Expr" expr
  use byteCode >>= log "ByteCode"
  use localScope >>= log "LocalScope"
  use globalScope >>= log "GlobalScope"
  codegenTop expr
