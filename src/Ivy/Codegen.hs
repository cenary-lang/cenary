{-# LANGUAGE OverloadedStrings #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Lens          hiding (op)
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map              as M
import qualified Data.Text             as T
--------------------------------------------------------------------------------
import           Ivy.Codegen.EvmAPI
import qualified Ivy.Codegen.EvmAPI.Op as Op
import           Ivy.Codegen.Types
import           Ivy.Parser
import           Ivy.Syntax
--------------------------------------------------------------------------------

codegenTop :: Expr -> Evm Integer

codegenTop (Times until (Block bodyExpr)) = do
  op2 PUSH1 until
  op JUMPDEST

  op PC
  op2 PUSH1 0x01
  op SWAP1
  op SUB
  op SWAP1
  op2 PUSH1 0x01
  op SWAP1
  op SUB
  mapM_ codegenTop bodyExpr
  op DUP1
  op SWAP2
  op JUMPI

  return 0

codegenTop (Assignment name val) = do
  symTable' <- use symTable
  addr <- codegenTop val
  case M.lookup name symTable' of
    Nothing ->       throwError $ VariableNotDefined name
    Just Nothing -> do
      op2 PUSH1 addr
      op MLOAD
      newAddr <- alloc
      op2 PUSH1 newAddr
      op MSTORE
      symTable %= M.update (const (Just (Just newAddr))) name
      return newAddr
    Just (Just oldAddr) -> do
      op2 PUSH1 addr
      op MLOAD
      op2 PUSH1 oldAddr
      op MSTORE
      symTable %= M.update (const (Just (Just oldAddr))) name
      return oldAddr

codegenTop (VarDecl name) =
  0 <$ (symTable %= M.insert name Nothing)

codegenTop (Identifier name) = do
  table <- use symTable
  case M.lookup name table of
    Nothing          -> throwError (VariableNotDeclared name)
    Just Nothing     -> throwError (VariableNotDefined name)
    Just (Just addr) -> return addr

codegenTop (PrimInt val) = do
  addr <- alloc
  op2 PUSH1 val
  op2 PUSH1 addr
  op MSTORE
  return addr

codegenTop (BinaryOp op expr1 expr2) = do
  left <- codegenTop expr1
  right <- codegenTop expr2
  case op of
    OpAdd -> binOp ADD left right
    OpMul -> binOp MUL left right
    OpSub -> binOp SUB left right
    OpDiv -> binOp DIV left right

codegen :: Expr -> WriterT T.Text (Either CodegenError) T.Text
codegen expr =
  _byteCode <$> execStateT (runEvm (codegenTop expr)) initCodegenState
