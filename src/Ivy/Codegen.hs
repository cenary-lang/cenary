{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Lens                   hiding (op)
import           Control.Monad.Except
import           Control.Monad.Logger.CallStack (logInfo)
import           Control.Monad.State
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import           Data.Monoid                    ((<>))
--------------------------------------------------------------------------------
import           Ivy.Codegen.EvmAPI
import qualified Ivy.Codegen.EvmAPI.Op          as Op
import           Ivy.Codegen.Types
import           Ivy.Parser
import           Ivy.Syntax
--------------------------------------------------------------------------------

useSymTable :: ScopeLevel -> Evm SymbolTable
useSymTable level = do
  tables <- use symTables
  case M.lookup level tables of
    Just t  -> return t
    Nothing -> M.empty <$ (symTables %= M.insert level M.empty)

updateSymTable :: ScopeLevel -> (SymbolTable -> SymbolTable) -> Evm ()
updateSymTable level f =
  symTables %= M.update (Just . f) level

initScopeLevel :: Int
initScopeLevel = 0

goInner :: ScopeLevel -> Evm ()
goInner level = do
  myTable <- useSymTable level
  logInfo "Updating sym table"
  updateSymTable (level + 1) (`M.union` myTable) -- union is left biased. Don't change its position!

codegenTop :: ScopeLevel -> Expr -> Evm (Maybe Integer)
codegenTop level (Times until (Block bodyExpr)) = do
  -- Assign target value
  op2 PUSH1 until
  op JUMPDEST

  -- Prepare true value of current PC
  op PC
  op2 PUSH1 0x01
  op SWAP1
  op SUB

  -- Decrease target value
  op SWAP1
  op2 PUSH1 0x01
  op SWAP1
  op SUB

  -- Code body
  goInner level
  forM_ bodyExpr $ codegenTop (level + 1)

  -- Jump to destination back if target value is nonzero
  op DUP1
  op SWAP2
  op JUMPI

  return Nothing

codegenTop level (Assignment name val) = do
  symTable' <- useSymTable level
  addr <- codegenTopOperand level val
  case M.lookup name symTable' of
    Nothing ->       throwError $ VariableNotDefined name
    Just Nothing -> do
      op2 PUSH1 addr
      op MLOAD
      newAddr <- alloc
      op2 PUSH1 newAddr
      op MSTORE
      updateSymTable level $ M.update (const (Just (Just newAddr))) name
      return Nothing
    Just (Just oldAddr) -> do
      op2 PUSH1 addr
      op MLOAD
      op2 PUSH1 oldAddr
      op MSTORE
      updateSymTable level $ M.update (const (Just (Just oldAddr))) name
      return Nothing

codegenTop level (VarDecl name) =
  Nothing <$ (updateSymTable level $ M.insert name Nothing)

codegenTop level (Identifier name) = do
  table <- useSymTable level
  case M.lookup name table of
    Nothing          -> throwError (VariableNotDeclared name)
    Just Nothing     -> throwError (VariableNotDefined name)
    Just (Just addr) -> return (Just addr)

codegenTop level (PrimInt val) = do
  addr <- alloc
  op2 PUSH1 val
  op2 PUSH1 addr
  op MSTORE
  return (Just addr)

codegenTop level (BinaryOp op expr1 expr2) = do
  left <- codegenTopOperand level expr1
  right <- codegenTopOperand level expr2
  case op of
    OpAdd -> binOp ADD left right
    OpMul -> binOp MUL left right
    OpSub -> binOp SUB left right
    OpDiv -> binOp DIV left right

codegenTopOperand :: ScopeLevel -> Expr -> Evm Integer
codegenTopOperand level expr =
  codegenTop level expr >>= \case
    Just val -> return val
    Nothing -> throwError $ InternalError $ "Following expression should have returned an operand, but it didn't: " <> show expr
