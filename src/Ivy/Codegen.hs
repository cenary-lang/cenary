{-# LANGUAGE OverloadedStrings #-}

module Ivy.Codegen where

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
import qualified Data.Map as M
--------------------------------------------------------------------------------
import qualified Ivy.Codegen.EvmAPI    as API
import qualified Ivy.Codegen.EvmAPI.Op as Op
import           Ivy.Codegen.Types
import           Ivy.Parser
import           Ivy.Syntax
--------------------------------------------------------------------------------

codegenTop :: Expr -> Evm Integer

codegenTop (Times until (Block bodyExpr)) = do
  body <- mapM codegenTop bodyExpr
  -- API.push1 until
  -- API.jumpdest
  -- API.push1 0x01
  -- API.swap1
  -- API.sub
  -- API.dup1
  -- API.
  -- PUSH x10
  -- JUMPDEST
  -- PUSH x01
  -- SWAP1
  -- SUB
  -- DUP1
  -- PUSH x02
  -- JUMPI
  return 0

codegenTop (Assignment name val) = do
  symTable' <- use symTable
  addr <- codegenTop val
  case M.lookup name symTable' of
    Nothing ->       throwError $ VariableNotDefined name
    Just Nothing -> do
      API.load addr
      newAddr <- API.alloc
      API.push1 newAddr
      API.run1 Op.mstore
      symTable %= M.update (const (Just (Just newAddr))) name
      return newAddr
    Just (Just oldAddr) -> do
      API.load addr
      API.push1 oldAddr
      API.run1 Op.mstore
      symTable %= M.update (const (Just (Just oldAddr))) name
      return oldAddr

codegenTop (VarDecl name) =
  0 <$ (symTable %= M.insert name Nothing)

codegenTop (Identifier name) = do
  table <- use symTable
  case M.lookup name table of
    Nothing -> throwError (VariableNotDeclared name)
    Just Nothing -> throwError (VariableNotDefined name)
    Just (Just addr) -> return addr

codegenTop (PrimInt val) = do
  addr <- API.alloc
  API.store addr val
  bc <- use byteCode
  return addr

codegenTop (BinaryOp op expr1 expr2) = do
  left <- codegenTop expr1
  right <- codegenTop expr2
  case op of
    OpAdd -> API.add left right
    OpMul -> API.mul left right
    OpSub -> API.sub left right
    OpDiv -> API.div left right

codegen :: Expr -> WriterT T.Text (Either CodegenError) T.Text
codegen expr =
  _byteCode <$> execStateT (runEvm (codegenTop expr)) initCodegenState
