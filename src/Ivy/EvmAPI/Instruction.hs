{-# LANGUAGE OverloadedStrings #-}

module Ivy.EvmAPI.Instruction where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Logger.CallStack (logInfo)
import           Control.Lens hiding (op)
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
import           Text.Printf
--------------------------------------------------------------------------------
import           Ivy.Codegen.Types
import qualified Ivy.Syntax             as S
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
  | MSTORE8
  | JUMP
  | JUMPI
  | PC
  | JUMPDEST
  | PUSH1
  | PUSH2
  | PUSH32
  | DUP1
  | DUP2
  | SWAP1
  | SWAP2
  | LOG0
  | LOG1
  | LOG2
  | RETURN
  | ADDRESS

toInstrCode :: Instruction -> Integer
toInstrCode STOP = 0x00
toInstrCode ADD = 0x01
toInstrCode MUL = 0x02
toInstrCode SUB = 0x03
toInstrCode DIV = 0x04
toInstrCode POP = 0x50
toInstrCode MLOAD = 0x51
toInstrCode MSTORE = 0x52
toInstrCode MSTORE8 = 0x53
toInstrCode JUMP = 0x56
toInstrCode JUMPI = 0x57
toInstrCode PC = 0x58
toInstrCode JUMPDEST = 0x5b
toInstrCode PUSH1 = 0x60
toInstrCode PUSH2 = 0x61
toInstrCode PUSH32 = 0x7f
toInstrCode DUP1 = 0x80
toInstrCode DUP2 = 0x81
toInstrCode SWAP1 = 0x90
toInstrCode SWAP2 = 0x91
toInstrCode LOG0 = 0xA0
toInstrCode LOG1 = 0xA1
toInstrCode LOG2 = 0xA2
toInstrCode RETURN = 0xf3
toInstrCode ADDRESS = 0x30

addBC :: Integer -> Evm ()
addBC val = byteCode <>= T.pack (printf "%064x" val)

op :: Instruction -> Evm ()
op instr = byteCode <>= T.pack (printf "%02x" (toInstrCode instr))

-- I forgot what this does after 10 minutes
op2 :: Instruction -> Integer -> Evm ()
op2 = curry ((op *** addBC) >>> uncurry (>>))
