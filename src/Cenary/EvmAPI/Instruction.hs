{-# LANGUAGE LambdaCase #-}

module Cenary.EvmAPI.Instruction
  ( Instruction (..)
  , toOpcode
  ) where

import Prelude hiding (EQ, LT, GT)

data Instruction =
    STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | MOD
  | GT
  | LT
  | EQ
  | ISZERO
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | JUMPDEST
  | CODECOPY
  | PUSH1 Integer
  | PUSH4 Integer
  | PUSH32 Integer
  | DUP1
  | EXP
  | CALLDATALOAD
  | DUP2
  | DUP3
  | DUP4
  | DUP5
  | DUP6
  | SWAP1
  | SWAP2
  | SWAP3
  | SWAP4
  | LOG0
  | LOG1
  | LOG2
  | RETURN
  | ADDRESS
  | SHA3
  deriving Show

toOpcode :: Instruction -> (Int, Integer)
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
  SLOAD        -> (0x54, 1)
  SSTORE       -> (0x55, 1)
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
  SWAP3        -> (0x92, 1)
  SWAP4        -> (0x93, 1)
  LOG0         -> (0xA0, 1)
  LOG1         -> (0xA1, 1)
  LOG2         -> (0xA2, 1)
  RETURN       -> (0xf3, 1)
  ADDRESS      -> (0x30, 1)
  SHA3         -> (0x20, 1)
