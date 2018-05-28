{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ivy.Codegen.Memory where

--------------------------------------------------------------------------------
import           Control.Lens hiding (ix, op)
import           Control.Monad
import           Data.Function (on)
import           Data.List (find, groupBy)
import qualified Data.Map as M
import           Data.Monoid
--------------------------------------------------------------------------------
import           Ivy.Codegen.Types
import           Ivy.EvmAPI.API
import           Ivy.Syntax
--------------------------------------------------------------------------------

class (Functor m, Applicative m, Monad m) => MemoryM m where
  load
    :: Integer
    -> VariablePersistence
    -> m ()
  load'
    :: VariablePersistence
    -> m ()
  alloc
    :: VariablePersistence
    -> m Integer
  store'
    :: VariablePersistence
    -> m ()
  push
    :: Integer
    -> m ()

instance MemoryM Evm where
  load addr persistence = do
    push32 addr
    load' persistence

  load' = \case
    Permanent -> sload
    Temporary -> mload

  store' = \case
    Permanent -> sstore
    Temporary -> mstore

  alloc = \case
    Permanent -> stackStorageEnd <<+= 0x20
    Temporary -> stackMemEnd <<+= 0x20

  push val =
    push32 val -- OPTIMIZE: different PUSH variants can be used for this task

totalMemBlockSize :: Integer
totalMemBlockSize = 32 -- There are 32 bytes in a block

calcAddr :: Integer -> Integer -> Integer
calcAddr ix allocLen = ix * totalMemBlockSize + allocLen

sizeof :: PrimType -> Size
sizeof TInt  = Size_8
sizeof TChar = Size_1
sizeof TBool = Size_1
sizeof other = error $ "`sizeof` is not implemented for type " <> show other

initMemory :: M.Map Integer Size
initMemory = M.empty

boolToInt :: Bool -> Integer
boolToInt True  = 1
boolToInt False = 0

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
sizeInt :: Size -> Integer
sizeInt _ = 0x20 -- TODO: Keep this until we have distinct sizes
-- sizeInt Size_1  = 1
-- sizeInt Size_2  = 2
-- sizeInt Size_4  = 4
-- sizeInt Size_8  = 8
-- sizeInt Size_32 = 32
