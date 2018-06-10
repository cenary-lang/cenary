{-# LANGUAGE TemplateHaskell #-}

module Cenary.Codegen.Operand where

import           Cenary.Syntax
import           Control.Lens

data Operand = Operand
  { _operandType :: PrimType
  }

makeLenses ''Operand
