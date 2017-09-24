{-# LANGUAGE OverloadedStrings #-}

module EvmL.Codegen.EvmAPI where

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
import           Text.Printf
--------------------------------------------------------------------------------
import qualified EvmL.Codegen.EvmAPI.Op as Op
import           EvmL.Codegen.Types
--------------------------------------------------------------------------------

push :: Integer -> Evm ()
push val = do
  run Op.push1
  run val

pop :: Evm ()
pop = run Op.pop

alloc :: Evm Integer
alloc = memPointer <+= 1

-- | Add given opcode to byteCode string
run :: Integer -> Evm ()
run val = do
  byteCode <>= T.pack (printf "%02d" val)

mul, add, sub, div :: Integer -> Integer -> Evm Integer
add = binOp Op.add
mul = binOp Op.mul
sub = binOp Op.sub
div = binOp Op.div

load :: Integer -> Evm ()
load n = do
  push n
  run Op.mload

store :: Integer -> Integer -> Evm ()
store addr val = do
  push val
  push addr
  run Op.mstore

binOp :: Integer -> (Integer -> Integer -> Evm Integer)
binOp op left right = do
  load left
  load right
  run op

  -- Store result and get its address
  addr <- alloc
  tell $ "Allocating for a binary operation result. Address: " <> T.pack (show addr) <> "\n"
  push addr
  run Op.mstore
  return addr
