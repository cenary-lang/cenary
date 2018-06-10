{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cenary.EvmAPI.API
  ( OpcodeM (..)
  , toOpcode
  , stop
  , add
  , mul
  , sub
  , div
  , mod
  , gt
  , lt
  , eq
  , iszero
  , pop
  , mload
  , mstore
  , mstore8
  , sload
  , sstore
  , jump
  , jumpi
  , codecopy
  , dup1
  , exp
  , calldataload
  , dup2
  , dup3
  , dup4
  , dup5
  , dup6
  , swap1
  , swap2
  , swap3
  , swap4
  , log0
  , log1
  , log2
  , op_return
  , address
  , push1
  , push4
  , push32
  , generateByteCode
  , inc
  , dec
  , sha3
  , leq
  , Instruction (JUMPDEST)
  ) where

import           Data.Monoid ((<>))
import           Cenary.EvmAPI.Instruction (Instruction (..))
import           Cenary.EvmAPI.Program (Program (..))
import           Prelude hiding (EQ, GT, LT, div, exp, mod)
import           Text.Printf
import           Cenary.EvmAPI.OpcodeM
import           Cenary.EvmAPI.Instruction

stop, add, mul, sub, div, mod, gt, lt, eq, iszero, pop, mload, mstore, mstore8, sload, sstore, jump, jumpi, codecopy, dup1, exp, calldataload, dup2, dup3, dup4, dup5, dup6, swap1, swap2, swap3, swap4, log0, log1, log2, op_return, address, sha3 :: OpcodeM m => m ()

stop         = op STOP
add          = op ADD
mul          = op MUL
sub          = op SUB
div          = op DIV
mod          = op MOD
gt           = op GT
lt           = op LT
eq           = op EQ
iszero       = op ISZERO
pop          = op POP
mload        = op MLOAD
mstore       = op MSTORE
mstore8      = op MSTORE8
sload        = op SLOAD
sstore       = op SSTORE
jump         = op JUMP
jumpi        = op JUMPI
codecopy     = op CODECOPY
dup1         = op DUP1
exp          = op EXP
calldataload = op CALLDATALOAD
dup2         = op DUP2
dup3         = op DUP3
dup4         = op DUP4
dup5         = op DUP5
dup6         = op DUP6
swap1        = op SWAP1
swap2        = op SWAP2
swap3        = op SWAP3
swap4        = op SWAP4
log0         = op LOG0
log1         = op LOG1
log2         = op LOG2
op_return    = op RETURN
address      = op ADDRESS
sha3         = op SHA3

push1 :: OpcodeM m => Integer -> m ()
push1 = op . PUSH1

push4 :: OpcodeM m => Integer -> m ()
push4 = op . PUSH4

push32 :: OpcodeM m => Integer -> m ()
push32 = op . PUSH32

generateByteCode :: Program -> String
generateByteCode (Program instructions) =
  foldr (\instr bc -> bc <> to_bytecode instr) "" instructions
  where
    to_bytecode :: Instruction -> String
    to_bytecode instr =
      let (opcode, _) = toOpcode instr
       in (printf "%02x" (opcode :: Int)) <> (pushed_vals instr)

    pushed_vals :: Instruction -> String
    pushed_vals = \case
                  PUSH32 val -> printf "%064x" val
                  PUSH4 val -> printf "%08x" val
                  PUSH1 val -> printf "%02x" val
                  _          -> ""

inc :: OpcodeM m => Integer -> m ()
inc val = push32 val >> add

dec :: OpcodeM m => Integer -> m ()
dec val = push32 val >> swap1 >> sub

leq :: OpcodeM m => m ()
leq = do
  push32 1
  swap1
  sub
  lt
