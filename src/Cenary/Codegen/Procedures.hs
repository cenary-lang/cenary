{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cenary.Codegen.Procedures where

import           Control.Lens hiding (op)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Semigroup ((<>))
import           Cenary.Codegen.Types
import           Cenary.EvmAPI.API
import           Cenary.Syntax

-- Input: [A: Operand Value]
-- Output: [keccak256 of input A]
applyHashingFunction :: CodegenM m => Integer -> PrimType -> m ()
applyHashingFunction order = \case
  TInt -> do
    -- [Value]
    valAddr <- alloc Local
    orderAddr <- alloc Local
    push32 valAddr -- [Value, ValAddr]
    store' Local -- []
    push32 order -- [Order]
    push32 orderAddr -- [Order, OrderAddr]
    store' Local -- []
    push32 0x40 -- [40]
    push32 valAddr -- [40, ValAddr]
    sha3 -- This instruction only works inside memory, not storage, unfortunately.
  TArray _ ->  do
    -- [HeapAddr]
    stackAddr <- alloc Local
    push32 stackAddr -- [HeapAddr, StackAddr]
    store' Local -- []
    push32 stackAddr -- [StackAddr]

    dup1 >> load' Local -- [StackAddr, HeapAddr]
    load' Local -- [StackAddr, ArrLength]
    inc 0x01 -- [StackAddr, ArrLength + 1]
    dup1 -- [StackAddr, ArrLength + 1, ArrLength + 1]
    swap2 -- [ArrLength + 1, ArrLength + 1, StackAddr]
    dup1 -- [ArrLength + 1, ArrLength + 1, StackAddr, StackAddr]
    swap2 -- [ArrLength + 1, StackAddr, StackAddr, ArrLength + 1]
    startResizingProcedure Local -- [ArrLength + 1, StackAddr]
    swap1 >> dup2 -- [StackAddr, ArrLength + 1, StackAddr]
    load' Local -- [StackAddr, ArrLength + 1, HeapAddr]
    dup2 -- [StackAddr, ArrLength + 1, HeapAddr, ArrLength + 1]
    push32 0x20 >> mul -- [StackAddr, ArrLength + 1, HeapAddr, 0x20 * (ArrLength + 1)]
    add -- [StackAddr, ArrLength + 1, HeapAddr + 0x20 * (ArrLength + 1)]
    push32 order -- [StackAddr, ArrLength + 1, HeapAddr + 0x20 * (ArrLength + 1), Order]
    swap1 -- [StackAddr, ArrLength + 1, Order, HeapAddr + 0x20 * (ArrLength + 1)]
    store' Local -- [StackAddr, ArrLength + 1]
    inc 0x01
    push32 0x20 >> mul -- [StackAddr, (ArrLength + 2) * 0x20]
    swap1 -- [(ArrLength + 2) * 0x20, StackAddr]
    load' Local -- [(ArrLength + 2) * 0x20, HeapAddr]
    sha3 -- [SHA3]
  otherTy -> throwError $ InternalError $ "Hashing function is not yet implemented for type " <> show otherTy

-- Input: [StackAddr, NewSize]
-- Output: []
startResizingProcedure
  :: CodegenM m
  => Scope
  -> m ()
startResizingProcedure persistence = do
  offset <- use funcOffset
  branchIfElse
    (offset)
    (do
      dup2 -- [StackAddr, NewSize, StackAddr]
      dup2 -- [StackAddr, NewSize, StackAddr, NewSize]
      swap1 -- [StackAddr, NewSize, NewSize, StackAddr]
      load' persistence -- [StackAddr, NewSize, NewSize, OldHeapAddr]
      load' persistence -- [StackAddr, NewSize, NewSize, OldSize]
      lt -- [StackAddr, NewSize, OldSize < NewSize]
    )
    -- new size is bigger, allocate new array space
    -- [StackAddr, NewSize] | NewSize > OldSize
    (expandArr offset persistence)
    -- [StackAddr, NewSize] | NewSize <= OldSize
    (swap1 >> load' persistence >> store' persistence) -- new size is smaller, just set the length identifier address

-- Input: [StackAddr, NewSize]
-- Output: []
expandArr :: CodegenM m => Integer -> Scope -> m ()
expandArr offset persistence = do
  -- [StackAddr, NewSize]
  rec heapBegin <- use heapSpaceBegin
      push32 heapBegin
      load' persistence -- [StackAddr, NewSize, NewAddr]
      swap1 -- [StackAddr, NewAddr, NewSize]
      dup2 -- [StackAddr, NewAddr, NewSize, NewAddr]
      store' persistence -- [StackAddr, NewAddr]
      swap1 -- [NewAddr, StackAddr]
      dup1 >> load' persistence -- [NewAddr, StackAddr, OldAddr]
      dup1 >> load' persistence -- [NewAddr, StackAddr, OldAddr, OldSize]

      loopBegin <- jumpdest

      -- Test for loop
      dup1 -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize]
      (push32 0 >> lt) -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize > 0]
      iszero -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize <= 0]
      push32 (loopEnd - offset) -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize <= 0, loopEnd]
      jumpi -- [NewAddr, StackAddr, OldAddr, OldSize]

      -- Loop body
      -- [NewAddr, StackAddr, OldAddr, OldSize]
      dup1 -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize]
      (push32 0x20 >> mul) -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize * 0x20]
      dup3 -- [NewAddr, StackAddr, OldAddr, OldSize, OldSize * 0x20, OldAddr]
      add -- [NewAddr, StackAddr, OldAddr, OldSize, 0x20 * OldSize + oldAddr]
      load' persistence -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1]]
      dup2 -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize]
      push32 0x20 >> mul -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize * 0x20]
      dup6 -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize * 0x20, NewAddr]
      add -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize * 0x20 + NewAddr]
      store' persistence -- [NewAddr, StackAddr, OldAddr, OldSize]
      dec 0x01 -- [NewAddr, StackAddr, OldAddr, OldSize - 1]

      -- Jump back to beginning of loop
      push32 (loopBegin - offset)
      jump

      -- End of the loop
      loopEnd <- jumpdest
      -- [NewAddr, StackAddr, OldAddr, 0]
      (pop >> pop) -- [NewAddr, StackAddr]
      -- swap1 >> dec 0x20 >> swap1 -- [NewAddr - 0x20, StackAddr]
      store' persistence -- []
  pure ()

jumpdest :: (OpcodeM m, MonadState CodegenState m) => m Integer
jumpdest = use pc <* op JUMPDEST

branchIf :: (OpcodeM m, MonadState CodegenState m, MonadFix m) => Integer -> m () -> m () -> m ()
branchIf offset loadPred ifComp = do
  rec loadPred
      iszero -- Jump if predicate is zero
      push32 (branchEnd - offset)
      jumpi
      ifComp
      branchEnd <- jumpdest
  pure ()

branchIfElse :: (OpcodeM m, MonadState CodegenState m, MonadFix m) => Integer -> m () -> m () -> m () -> m ()
branchIfElse offset loadPred ifComp elseComp = do
  rec loadPred
      push32 (elseEndIfBegin - offset)
      jumpi
      elseComp
      push32 (branchEnd - offset)
      jump
      elseEndIfBegin <- jumpdest
      ifComp
      branchEnd <- jumpdest
  pure()

