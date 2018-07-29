{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cenary.Codegen.Procedures where

import           Cenary.Codegen.CodegenError
import           Cenary.Codegen.Types
import           Cenary.EvmAPI.API
import           Cenary.Syntax
import qualified Control.Category as Cat
import           Control.Lens hiding (op)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Semigroup ((<>))
import           Prelude

-- Input: [A: Operand Value]
-- Output: [keccak256 of input A]
applyHashingFunction :: CodegenM m => Integer -> PrimType -> m ()
applyHashingFunction order = \case
  TInt -> do
    -- [Value]
    valAddr <- alloc Local
    orderAddr <- alloc Local
    sequence_ [push32 valAddr, store_ Local]
    -- push32 valAddr -- [Value, ValAddr]
    -- store_ Local -- []
    push32 order -- [Order]
    push32 orderAddr -- [Order, OrderAddr]
    store_ Local -- []
    push32 0x40 -- [40]
    push32 valAddr -- [40, ValAddr]
    sha3 -- This instruction only works inside memory, not storage, unfortunately.
  TArray _ ->  do
    -- [HeapAddr]
    stackAddr <- alloc Local
    push32 stackAddr -- [HeapAddr, StackAddr]
    store_ Local -- []
    push32 stackAddr -- [StackAddr]

    dup1 >> load_ Local -- [StackAddr, HeapAddr]
    load_ Local -- [StackAddr, ArrLength]
    inc 0x01 -- [StackAddr, ArrLength + 1]
    dup1 -- [StackAddr, ArrLength + 1, ArrLength + 1]
    swap2 -- [ArrLength + 1, ArrLength + 1, StackAddr]
    dup1 -- [ArrLength + 1, ArrLength + 1, StackAddr, StackAddr]
    swap2 -- [ArrLength + 1, StackAddr, StackAddr, ArrLength + 1]
    startResizingProcedure Local -- [ArrLength + 1, StackAddr]
    swap1 >> dup2 -- [StackAddr, ArrLength + 1, StackAddr]
    load_ Local -- [StackAddr, ArrLength + 1, HeapAddr]
    dup2 -- [StackAddr, ArrLength + 1, HeapAddr, ArrLength + 1]
    push32 0x20 >> mul -- [StackAddr, ArrLength + 1, HeapAddr, 0x20 * (ArrLength + 1)]
    add -- [StackAddr, ArrLength + 1, HeapAddr + 0x20 * (ArrLength + 1)]
    push32 order -- [StackAddr, ArrLength + 1, HeapAddr + 0x20 * (ArrLength + 1), Order]
    swap1 -- [StackAddr, ArrLength + 1, Order, HeapAddr + 0x20 * (ArrLength + 1)]
    store_ Local -- [StackAddr, ArrLength + 1]
    inc 0x01
    push32 0x20 >> mul -- [StackAddr, (ArrLength + 2) * 0x20]
    swap1 -- [(ArrLength + 2) * 0x20, StackAddr]
    load_ Local -- [(ArrLength + 2) * 0x20, HeapAddr]
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
      load_ persistence -- [StackAddr, NewSize, NewSize, OldHeapAddr]
      load_ persistence -- [StackAddr, NewSize, NewSize, OldSize]
      lt -- [StackAddr, NewSize, OldSize < NewSize]
    )
    -- new size is bigger, allocate new array space
    -- [StackAddr, NewSize] | NewSize > OldSize
    (expandArr offset persistence)
    -- [StackAddr, NewSize] | NewSize <= OldSize
    (swap1 >> load_ persistence >> store_ persistence) -- new size is smaller, just set the length identifier address

-- Input: [StackAddr, NewSize]
-- Output: []
expandArr :: CodegenM m => Integer -> Scope -> m ()
expandArr offset persistence = do
  -- [StackAddr, NewSize]
  rec heapBegin <- use heapSpaceBegin
      push32 heapBegin
      load_ persistence -- [StackAddr, NewSize, NewAddr]
      swap1 -- [StackAddr, NewAddr, NewSize]
      dup2 -- [StackAddr, NewAddr, NewSize, NewAddr]
      store_ persistence -- [StackAddr, NewAddr]
      swap1 -- [NewAddr, StackAddr]
      dup1 >> load_ persistence -- [NewAddr, StackAddr, OldAddr]
      dup1 >> load_ persistence -- [NewAddr, StackAddr, OldAddr, OldSize]

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
      load_ persistence -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1]]
      dup2 -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize]
      push32 0x20 >> mul -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize * 0x20]
      dup6 -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize * 0x20, NewAddr]
      add -- [NewAddr, StackAddr, OldAddr, OldSize, oldAddr[size-1], OldSize * 0x20 + NewAddr]
      store_ persistence -- [NewAddr, StackAddr, OldAddr, OldSize]
      dec 0x01 -- [NewAddr, StackAddr, OldAddr, OldSize - 1]

      -- Jump back to beginning of loop
      push32 (loopBegin - offset)
      jump

      -- End of the loop
      loopEnd <- jumpdest
      -- [NewAddr, StackAddr, OldAddr, 0]
      (pop >> pop) -- [NewAddr, StackAddr]
      -- swap1 >> dec 0x20 >> swap1 -- [NewAddr - 0x20, StackAddr]
      store_ persistence -- []
  pure ()

jumpdest :: (OpcodeM m, MonadState CodegenState m) => m Integer
jumpdest = use pc <* op JUMPDEST

mycomp :: OpcodeM m => Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs) -> Integer -> Integer -> m ()
mycomp loadPred offset branchEnd = unsafeGen $ do
  loadPred
  iszero'
  push32' @'Destination (branchEnd - offset)
  jumpi'
  where
    (>>) = (Cat.>>>)

mycomp'
  :: OpcodeM m
  => Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
  -> Integer
  -> Integer
  -> Instr xs b
  -> m ()
mycomp' loadPred offset branchEnd ifComp = unsafeGen $ do
  loadPred
  iszero'
  push32' @'Destination (branchEnd - offset)
  jumpi'
  ifComp
  where
    (>>) = (Cat.>>>)

branchIf :: (OpcodeM m, MonadState CodegenState m, MonadFix m) => Integer -> Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs) -> m () -> m ()
branchIf offset loadPred ifComp = do
  rec (mycomp loadPred offset branchEnd)
      ifComp
      branchEnd <- jumpdest
  pure ()

branchIf'
  :: (OpcodeM m, MonadState CodegenState m, MonadFix m)
  => Integer
  -> Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
  -> Instr xs b
  -> m ()
branchIf' offset loadPred ifComp = do
  rec (mycomp' loadPred offset branchEnd ifComp)
      branchEnd <- jumpdest
  pure ()

empty :: Instr a b
empty = Instr id

-- branchIf' :: Integer -> Instr xs (x ': xs) -> Instr (x ': xs) ys -> Instr xs ys
-- branchIf' offset loadPred ifComp = do
--   rec loadPred
--       iszero' -- Jump if predicate is zero
--       push32' (branchEnd - offset)
--       jumpi'
--       ifComp
--       branchEnd <- jumpdest'
--   empty
--   where
--     (>>) = (Cat.>>>)

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

