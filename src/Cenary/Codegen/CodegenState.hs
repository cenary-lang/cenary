{-# LANGUAGE TemplateHaskell #-}

module Cenary.Codegen.CodegenState where

import           Cenary.Codegen.Address
import           Cenary.Codegen.Env
import           Cenary.Codegen.FuncRegistry
import qualified Cenary.Codegen.MappingOrder as MO
import           Cenary.EvmAPI.Program
import           Control.Lens

data CodegenState = CodegenState
  { _env             :: !(Env Address)
  , _heapSpaceBegin  :: Integer -- Dis boi should stay lazy or bad things happen
  , _sp              :: !Integer -- Stack pointer
  , _framePtrs       :: ![Integer]
  , _maxStackSize    :: Integer
  , _stackStorageEnd :: !Integer
  , _pc              :: !Integer
  , _funcRegistry    :: !FuncRegistry
  , _program         :: Program
  , _funcOffset      :: !Integer
  , _mappingOrder    :: MO.MappingOrder
  }

makeLenses ''CodegenState


