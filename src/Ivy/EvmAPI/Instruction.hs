{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ivy.EvmAPI.Instruction where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Logger.CallStack (logInfo)
import           Control.Lens hiding (op)
import           Control.Monad.State
import Data.Monoid
import           Control.Monad.Writer
import qualified Data.Text              as T
import           Text.Printf
import           Prelude hiding (LT, EQ, GT)
import           GHC.TypeLits
import Data.Proxy
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
  deriving Show

type Opcode = Integer

data StackUpdate :: Nat -> Nat -> *

type family Weight (instr :: Instruction) where
  Weight 'STOP     = StackUpdate 0 0
  Weight 'ADD      = StackUpdate 2 1
  Weight 'MUL      = StackUpdate 2 1
  Weight 'SUB      = StackUpdate 2 1
  Weight 'DIV      = StackUpdate 2 1
  Weight 'MOD      = StackUpdate 2 1
  Weight 'GT       = StackUpdate 2 1
  Weight 'LT       = StackUpdate 2 1
  Weight 'EQ       = StackUpdate 2 1
  Weight 'ISZERO   = StackUpdate 1 1
  Weight 'POP      = StackUpdate 1 0
  Weight 'MLOAD    = StackUpdate 1 1
  Weight 'MSTORE   = StackUpdate 2 0
  Weight 'MSTORE8  = StackUpdate 2 0
  Weight 'JUMP     = StackUpdate 1 0
  Weight 'JUMPI    = StackUpdate 2 1
  Weight 'PC       = StackUpdate 0 1
  Weight 'JUMPDEST = StackUpdate 0 0
  Weight 'PUSH1    = StackUpdate 0 1
  Weight 'PUSH2    = StackUpdate 0 1
  Weight 'PUSH32   = StackUpdate 0 1
  Weight 'DUP1     = StackUpdate 1 2
  Weight 'DUP2     = StackUpdate 2 3
  Weight 'SWAP1    = StackUpdate 2 2
  Weight 'SWAP2    = StackUpdate 3 3
  Weight 'LOG0     = StackUpdate 2 0
  Weight 'LOG1     = StackUpdate 3 0
  Weight 'LOG2     = StackUpdate 4 0
  Weight 'RETURN   = StackUpdate 2 0
  Weight 'ADDRESS  = StackUpdate 0 1

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
  PUSH2    -> (0x61, 3)
  PUSH32   -> (0x7f, 3)
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

op' :: (Weight instr1 ~ Weight instr2) => Proxy instr1 -> Proxy instr2 -> Instruction -> Instruction -> String
op' _ _ instr1 instr2 = show instr1

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
