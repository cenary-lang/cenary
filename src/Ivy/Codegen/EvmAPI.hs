{-# LANGUAGE OverloadedStrings #-}

module Ivy.Codegen.EvmAPI where

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
import           Text.Printf
--------------------------------------------------------------------------------
import qualified Ivy.Codegen.EvmAPI.Op as Op
import           Ivy.Codegen.Types
--------------------------------------------------------------------------------

push2 :: Integer -> Evm ()
push2 val = do
  run1 Op.push2
  run2 val

push1 :: Integer -> Evm ()
push1 val = do
  run1 Op.push1
  run1 val

pop :: Evm ()
pop = run1 Op.pop

alloc :: Evm Integer
alloc = memPointer <+= 32

-- | Add given opcode to byteCode string
run2 :: Integer -> Evm ()
run2 val = do
  byteCode <>= T.pack (printf "%064x" val)

run1 :: Integer -> Evm ()
run1 val = do
  byteCode <>= T.pack (printf "%02x" val)

mul, add, sub, div :: Integer -> Integer -> Evm Integer
add = binOp Op.add
mul = binOp Op.mul
sub = binOp Op.sub
div = binOp Op.div

load :: Integer -> Evm ()
load addr = do
  push1 addr
  run1 Op.mload

store :: Integer -> Integer -> Evm ()
store addr val = do
  push2 val
  push1 addr
  run1 Op.mstore

binOp :: Integer -> (Integer -> Integer -> Evm Integer)
binOp op left right = do
  load left
  load right
  run1 op

  -- Store result and get its address
  addr <- alloc
  tell $ "Allocating for a binary operation result. Address: " <> T.pack (show addr) <> "\n"
  push1 addr
  run1 Op.mstore
  return addr
