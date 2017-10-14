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

addBC :: Integer -> Evm ()
addBC val = byteCode <>= T.pack (printf "%064x" val)

op :: Instruction -> Evm ()
op instr = byteCode <>= T.pack (printf "%02x" (toInstrCode instr))

-- I forgot what this does after 10 minutes
op2 :: Instruction -> Integer -> Evm ()
op2 = curry ((op *** addBC) >>> uncurry (>>))

load
  :: Size
  -> Address
  -> Evm ()
load size addr = do
  op2 PUSH32 (0x10 ^ (64 - 2 * sizeInt size))
  logInfo $ "Loading with size: " <> T.pack (show (sizeInt size))
  op2 PUSH32 addr
  op MLOAD
  op DIV

{-|
Given the size and necessary stack state, stores that much byte properly aligned.
For instance, we want to store 2 bytes of data at 0x0000, so we have the following stack:

00000000
327024A6

And the iterative process:

00000003
327024A6

-> MSTORE8

00000002
00327024

-> MSTORE8

00000001
00003270

-> MSTORE8

00000000
00000032

-> MSTORE8

-> EXIT

-}
storeMultibyte
  :: Size
  -> Evm ()
storeMultibyte size = do
  op2 PUSH32 (sizeInt size - 1)
  op ADD
  replicateM_ (fromIntegral (sizeInt size)) $ do
    -- Populate value and address for the next iteration
    op DUP2
    op DUP2

    -- Actually store the value
    op MSTORE8

    -- Decrease position by one to go left one position
    op2 PUSH32 0x01
    op SWAP1
    op SUB

    -- Shift number 8 bits right
    op2 PUSH32 0x100
    op SWAP1
    op SWAP2
    op DIV

    -- Swap to restore stack position which is like (address, value) instead of (value, address)
    op SWAP1

  -- Cleanup
  op POP
  op POP

storeAddressed
  :: Size    -- Variable size
  -> Integer -- Address of the value. Value should be loaded from this address
  -> Integer -- Address to put value on
  -> Evm ()
storeAddressed size valAddr destAddr = do
  -- Initial state
  load size valAddr
  op2 PUSH32 destAddr
  
  storeMultibyte size

storeVal
  :: Size    -- Variable size
  -> Integer -- Actual value
  -> Integer -- Address to put value on
  -> Evm ()
storeVal size val destAddr = do
  let endDest = sizeInt size + destAddr - 1 -- To store 8 byte on address 10, we start from 17 and go back to 10
  op2 PUSH32 val
  op2 PUSH32 endDest
  op MSTORE8
  forM_ [1..sizeInt size - 1] $ \i -> do
    op2 PUSH32 (val `div` (0x100 ^ i))
    op2 PUSH32 (endDest - i)
    op MSTORE8

binOp :: S.PrimType -> Instruction -> Integer -> Integer -> Evm (Maybe Operand)
binOp t instr left right = do
  load (sizeof S.IntT) left
  load (sizeof S.IntT) right
  op instr
  addr <- alloc (sizeof S.IntT)
  logInfo $ "Address: " <> T.pack (show addr)
  op2 PUSH32 addr
  storeMultibyte (sizeof S.IntT)
  return (Just (Operand t addr))
