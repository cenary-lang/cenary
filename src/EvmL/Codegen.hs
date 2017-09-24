{-# LANGUAGE OverloadedStrings #-}

module EvmL.Codegen where

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
import qualified Data.Map as M
--------------------------------------------------------------------------------
import qualified EvmL.Codegen.EvmAPI    as API
import qualified EvmL.Codegen.EvmAPI.Op as Op
import           EvmL.Codegen.Types
import           EvmL.Parser
import           EvmL.Syntax
--------------------------------------------------------------------------------

data EvmCode =
  EvmInt Int

codegenTop :: Expr -> Evm Integer

codegenTop (Assignment name val) = do
  symTable' <- use symTable
  addr <- codegenTop val
  case M.lookup name symTable' of
    Nothing ->       throwError $ VariableNotDefined name
    Just _oldAddr -> addr <$ (symTable %= M.update (const (Just (Just addr))) name)

codegenTop (VarDecl name) = do
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
    OpAdd -> tell "add\n" >> API.add left right
    OpMul -> tell "mul\n" >> API.mul left right
    OpSub -> tell "sub\n" >> API.sub left right
    OpDiv -> tell "div\n" >> API.div left right

codegen :: Expr -> WriterT T.Text (Either CodegenError) T.Text
codegen expr =
  _byteCode <$> execStateT (runEvm (codegenTop expr)) initCodegenState
