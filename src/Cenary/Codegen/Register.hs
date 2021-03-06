module Cenary.Codegen.Register
  ( allocRegisters
  , storeReg
  , loadReg
  , storeRegVal
  , Register (..)
  ) where

import           Cenary.Codegen.Types
import           Cenary.EvmAPI.API
import           Data.Foldable (for_)
import           Data.Functor (void)

data Register =
    Reg_A
  | Reg_FunCall
  | Reg_B
  | Reg_C
  | Reg_D
  deriving (Enum, Bounded)

regAddr :: Register -> Integer
regAddr = (* 0x20) . fromIntegral . fromEnum

allocRegisters :: MemoryM m => m ()
allocRegisters = do
  for_ [(minBound :: Register)..maxBound] $ const (void (alloc Local))

storeReg :: (OpcodeM m, MemoryM m) => Register -> m ()
storeReg reg = push32 (regAddr reg) >> mstore

loadReg :: (OpcodeM m, MemoryM m) => Register -> m ()
loadReg reg = push32 (regAddr reg) >> mload

storeRegVal :: (OpcodeM m, MemoryM m) => Register -> Integer -> m ()
storeRegVal reg val = push32 val >> storeReg reg
