{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns     #-}

module Ivy.EvmAPI.Instruction where

--------------------------------------------------------------------------------
import           Control.Lens hiding (op)
import           Control.Monad.Except
import           Control.Monad.State (MonadState)
import           Data.Monoid ((<>))
import           Prelude hiding (EQ, GT, LT)
import           Text.Printf
--------------------------------------------------------------------------------
import           Ivy.Codegen.Types
--------------------------------------------------------------------------------

type Opcode = Integer

toInstrCode :: Instruction -> (Opcode, Integer)
toInstrCode = \case
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
  PC           -> (0x58, 1)
  JUMPDEST     -> (0x5b, 1)
  CODECOPY     -> (0x39, 1)
  PUSH1 _      -> (0x60, 1 + 1)
  PUSH4 _      -> (0x63, 1 + 4)
  PUSH32 _     -> (0x7f, 1 + 32)
  DUP1         -> (0x80, 1)
  EXP          -> (0x0a, 1)
  CALLDATALOAD -> (0x35, 1)
  DUP2         -> (0x81, 1)
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
  -- ^ Opcode without argument

generateByteCode :: Program -> String
generateByteCode (Program instructions) =
  foldr (\instr bc -> bc <> to_bytecode instr) "" instructions
  where
    to_bytecode :: Instruction -> String
    to_bytecode instr =
      let (opcode, _) = toInstrCode instr
       in (printf "%02x" opcode) <> pushed_vals
      where
        pushed_vals :: String
        pushed_vals = case instr of
                      PUSH32 val -> printf "%064x" val
                      PUSH4 val -> printf "%08x" val
                      PUSH1 val -> printf "%02x" val
                      _          -> ""

instance OpcodeM Evm where
  op instr = do
    let (_, cost) = toInstrCode instr
    pc += cost
    program %= (addInstr instr)

jumpdest :: (OpcodeM m, MonadState CodegenState m) => m Integer
jumpdest = use pc <* op JUMPDEST
