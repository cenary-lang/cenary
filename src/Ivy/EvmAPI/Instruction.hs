{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Ivy.EvmAPI.Instruction where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Monad.Except
import           Control.Lens hiding (op)
import qualified Data.Text              as T
import           Text.Printf
import           Prelude hiding (LT, EQ, GT)
--------------------------------------------------------------------------------
import           Ivy.Codegen.Types
--------------------------------------------------------------------------------

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
  | JUMP
  | JUMPI
  | PC
  | JUMPDEST
  | PUSH1
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
  deriving Show

type Opcode = Integer

toInstrCode :: Instruction -> (Opcode, Integer)
toInstrCode = \case
  STOP     -> (0x00, 1)
  ADD      -> (0x01, 1)
  MUL      -> (0x02, 1)
  SUB      -> (0x03, 1)
  DIV      -> (0x04, 1)
  MOD      -> (0x06, 1)
  GT       -> (0x11, 1)
  LT       -> (0x10, 1)
  EQ       -> (0x14, 1)
  ISZERO   -> (0x15, 1)
  POP      -> (0x50, 1)
  MLOAD    -> (0x51, 1)
  MSTORE   -> (0x52, 1)
  MSTORE8  -> (0x53, 1)
  JUMP     -> (0x56, 1)
  JUMPI    -> (0x57, 1)
  PC       -> (0x58, 1)
  JUMPDEST -> (0x5b, 1)
  PUSH1    -> (0x60, 2)
  PUSH32   -> (0x7f, 33)
  DUP1     -> (0x80, 1)
  DUP2     -> (0x81, 1)
  SWAP1    -> (0x90, 1)
  SWAP2    -> (0x91, 1)
  LOG0     -> (0xA0, 1)
  LOG1     -> (0xA1, 1)
  LOG2     -> (0xA2, 1)
  RETURN   -> (0xf3, 1)
  ADDRESS  -> (0x30, 1)

pcCost :: Instruction -> Integer
pcCost (toInstrCode -> (_, cost)) = cost

pcCosts :: [Instruction] -> Integer
pcCosts = sum . map pcCost

-- | Class of monads that can run opcodes
class Monad m => OpcodeM m where
  op :: Instruction -> m ()
  -- ^ Opcode without argument
  op2 :: Instruction -> Integer -> m ()
  -- ^ Opcode with exactly one argument
  addBC :: Integer -> m ()
  -- ^ Add bytecode

instance OpcodeM Evm where
  op instr = do
    let (opcode, pcIncr) = toInstrCode instr
    pc += pcIncr
    byteCode <>= T.pack (printf "%02x" opcode)
  op2 = curry ((op *** addBC) >>> uncurry (>>))
  addBC val =
    byteCode <>= T.pack (printf "%064x" val)
