{-# LANGUAGE OverloadedStrings #-}

module EvmL.Codegen where

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
--------------------------------------------------------------------------------
import qualified EvmL.Codegen.EvmAPI    as API
import qualified EvmL.Codegen.EvmAPI.Op as Op
import           EvmL.Codegen.Types
import           EvmL.Parser
import           EvmL.Syntax
--------------------------------------------------------------------------------

data EvmCode =
  EvmInt Int

intExpr :: Expr -> Evm Integer
intExpr (PrimInt int) = return int
intExpr _             = throwError MeaninglessExpr

codegenTop :: Expr -> Evm Integer
codegenTop (PrimInt val) = do
  addr <- API.alloc
  API.store addr val
  bc <- use byteCode
  tell $ "Allocating for a prim int. Address: " <> T.pack (show addr) <> "\n"
  tell $ "[B] " <> bc <> "\n"
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
