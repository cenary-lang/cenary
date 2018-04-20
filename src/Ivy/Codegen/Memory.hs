{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ivy.Codegen.Memory
  ( MemoryM (..)
  , sizeof
  , boolToInt
  , sizeInt
  )
  where

--------------------------------------------------------------------------------
import           Control.Lens hiding (ix, op)
import           Data.Monoid
--------------------------------------------------------------------------------
import           Ivy.Codegen.Types
import           Ivy.EvmAPI.API
import           Ivy.Syntax
--------------------------------------------------------------------------------

class MemoryM m where
  storeAddressed
    :: Integer -- Address of the value. Value should be loaded from this address
    -> Integer -- Address to put value on
    -> m ()
  load
    :: Integer
    -> m ()
  storeVal
    :: Integer -- Actual value
    -> Integer -- Address to put value on
    -> m ()
  alloc
    :: m Integer
  store
    :: m ()
  push
    :: Integer
    -> m ()
  allocBulk
    :: Integer
    -> m Integer

instance MemoryM Evm where
  storeAddressed valAddr destAddr = do
    -- Initial state
    load valAddr
    push32 destAddr
    store

  load addr = do
    push32 addr
    mload

  storeVal val destAddr = do
    push32 val
    push32 destAddr
    store

  store =
    mstore

  alloc =
    stackMemSize <<+= 0x20

  push =
    push32 -- OPTIMIZE: different PUSH variants can be used for this task

  allocBulk len =
    stackMemSize <<+= (0x20 * len)

sizeof :: PrimType -> Size
sizeof TInt  = Size_8
sizeof TChar = Size_1
sizeof TBool = Size_1
sizeof other = error $ "`sizeof` is not implemented for type " <> show other

boolToInt :: Bool -> Integer
boolToInt True  = 1
boolToInt False = 0

sizeInt :: Size -> Integer
sizeInt Size_1  = 1
sizeInt Size_2  = 2
sizeInt Size_4  = 4
sizeInt Size_8  = 8
sizeInt Size_32 = 32
