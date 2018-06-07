module Cenary.EvmAPI.Instruction
  (Instruction (..)
  ) where

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

