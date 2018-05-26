{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivy.EvmAPI.API
  ( OpcodeM
  , jumpdest
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
  ) where

import           Control.Lens hiding (op)
import           Control.Monad.State (MonadState)
import           Data.Monoid ((<>))
import           Ivy.Codegen.Types
import           Prelude hiding (EQ, GT, LT, div, exp, mod)
import           Text.Printf

type Opcode = Integer

toOpcode :: Instruction -> (Opcode, Integer)
toOpcode = \case
  STOP         -> (0x00, 1)
  ADD          -> (0x01, 1)
  MUL          -> (0x02, 1)
  SUB          -> (0x03, 1)
  DIV          -> (0x04, 1)
  MOD          -> (0x06, 1)
  GT           -> (0x11, 1)
  LT           -> (0x10, 1)
  EQ           -> (0x14, 1)
  ISZERO       -> (0x15, 1)
  POP          -> (0x50, 1)
  MLOAD        -> (0x51, 1)
  MSTORE       -> (0x52, 1)
  MSTORE8      -> (0x53, 1)
  JUMP         -> (0x56, 1)
  JUMPI        -> (0x57, 1)
  JUMPDEST     -> (0x5b, 1)
  CODECOPY     -> (0x39, 1)
  PUSH1 _      -> (0x60, 1 + 1)
  PUSH4 _      -> (0x63, 1 + 4)
  PUSH32 _     -> (0x7f, 1 + 32)
  DUP1         -> (0x80, 1)
  EXP          -> (0x0a, 1)
  CALLDATALOAD -> (0x35, 1)
  DUP2         -> (0x81, 1)
  DUP3         -> (0x82, 1)
  DUP4         -> (0x83, 1)
  DUP5         -> (0x84, 1)
  DUP6         -> (0x85, 1)
  SWAP1        -> (0x90, 1)
  SWAP2        -> (0x91, 1)
  LOG0         -> (0xA0, 1)
  LOG1         -> (0xA1, 1)
  LOG2         -> (0xA2, 1)
  RETURN       -> (0xf3, 1)
  ADDRESS      -> (0x30, 1)

-- | Class of monads that can run opcodes
class Monad m => OpcodeM m where
  op :: Instruction -> m ()

instance OpcodeM Evm where
  op instr = do
    let (_, cost) = toOpcode instr
    pc += cost
    program %= (addInstr instr)

stop, add, mul, sub, div, mod, gt, lt, eq, iszero, pop, mload, mstore, mstore8, jump, jumpi, codecopy, dup1, exp, calldataload, dup2, dup3, dup4, dup5, dup6, swap1, swap2, log0, log1, log2, op_return, address :: OpcodeM m => m ()

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
log0         = op LOG0
log1         = op LOG1
log2         = op LOG2
op_return    = op RETURN
address      = op ADDRESS

push1 :: OpcodeM m => Integer -> m ()
push1 = op . PUSH1

push4 :: OpcodeM m => Integer -> m ()
push4 = op . PUSH4

push32 :: OpcodeM m => Integer -> m ()
push32 = op . PUSH32

jumpdest :: (OpcodeM m, MonadState CodegenState m) => m Integer
jumpdest = use pc <* op JUMPDEST

generateByteCode :: Program -> String
generateByteCode (Program instructions) =
  foldr (\instr bc -> bc <> to_bytecode instr) "" instructions
  where
    to_bytecode :: Instruction -> String
    to_bytecode instr =
      let (opcode, _) = toOpcode instr
       in (printf "%02x" opcode) <> (pushed_vals instr)

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
