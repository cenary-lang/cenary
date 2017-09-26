{-# LANGUAGE OverloadedStrings #-}

module Ivy.Codegen.EvmAPI where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Applicative
import           Control.Lens hiding (op)
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
import           Text.Printf
--------------------------------------------------------------------------------
import qualified Ivy.Codegen.EvmAPI.Op as Op
import           Ivy.Codegen.Types
--------------------------------------------------------------------------------

data Instruction =
    STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | POP
  | MLOAD
  | MSTORE
  | JUMP
  | JUMPI
  | PC
  | JUMPDEST
  | PUSH1
  | PUSH2
  | PUSH32
  | DUP1
  | SWAP1
  | SWAP2
  | RETURN

toInstrCode :: Instruction -> Integer
toInstrCode STOP = 0x00
toInstrCode ADD = 0x01
toInstrCode MUL = 0x02
toInstrCode SUB = 0x03
toInstrCode DIV = 0x04
toInstrCode POP = 0x50
toInstrCode MLOAD = 0x51
toInstrCode MSTORE = 0x52
toInstrCode JUMP = 0x56
toInstrCode JUMPI = 0x57
toInstrCode PC = 0x58
toInstrCode JUMPDEST = 0x5b
toInstrCode PUSH1 = 0x60
toInstrCode PUSH2 = 0x61
toInstrCode PUSH32 = 0x7f
toInstrCode DUP1 = 0x80
toInstrCode SWAP1 = 0x90
toInstrCode SWAP2 = 0x91
toInstrCode RETURN = 0xf3

addBC :: Integer -> Evm ()
addBC val = byteCode <>= T.pack (printf "%02x" val)

op :: Instruction -> Evm ()
op instr = byteCode <>= T.pack (printf "%02x" (toInstrCode instr))

op2 :: Instruction -> Integer -> Evm ()
op2 = curry ((op *** addBC) >>> uncurry (>>))

alloc :: Evm Integer
alloc = memPointer <+= 32

binOp :: Instruction -> Integer -> Integer -> Evm Integer
binOp instr left right = do
  op2 PUSH1 left
  op MLOAD
  op2 PUSH1 right
  op MLOAD
  op instr
  addr <- alloc
  op2 PUSH1 addr
  op MSTORE
  return addr

-- -- | Add given opcode to byteCode string
-- run2 :: Integer -> Evm ()
-- run2 val = do
--   byteCode <>= T.pack (printf "%064x" val)

-- run1 :: Integer -> Evm ()
-- run1 val = do
--   byteCode <>= T.pack (printf "%02x" val)

-- mul, add, sub, div :: Integer -> Integer -> Evm Integer
-- add = binOp Op.add
-- mul = binOp Op.mul
-- sub = binOp Op.sub
-- div = binOp Op.div

-- load :: Integer -> Evm ()
-- load addr = do
--   push1 addr
--   run1 Op.mload

-- store :: Integer -> Integer -> Evm ()
-- store addr val = do
--   push2 val
--   push1 addr
--   run1 Op.mstore

-- binOp :: Integer -> (Integer -> Integer -> Evm Integer)
-- binOp op left right = do
--   load left
--   load right
--   run1 op

--   -- Store result and get its address
--   addr <- alloc
--   tell $ "Allocating for a binary operation result. Address: " <> T.pack (show addr) <> "\n"
--   push1 addr
--   run1 Op.mstore
--   return addr
