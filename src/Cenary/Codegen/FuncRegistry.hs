{-# LANGUAGE TemplateHaskell #-}

module Cenary.Codegen.FuncRegistry where

import           Cenary.Syntax
import           Control.Lens
import qualified Data.Map as M

data FuncRegistryArgInfo = FuncRegistryArgInfo
  { _argTy   :: PrimType
  , _argName :: String
  }

data FuncRegistry = FuncRegistry
  { _argsAddresses :: !(M.Map String [FuncRegistryArgInfo])
  }

makeLenses ''FuncRegistry
