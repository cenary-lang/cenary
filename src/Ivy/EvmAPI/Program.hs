module Ivy.EvmAPI.Program
  ( Program (..)
  ) where

import Ivy.EvmAPI.Instruction (Instruction (..))
import qualified Data.Sequence as Seq

-- | TODO: This should have its own module
newtype Program = Program { _unProgram :: (Seq.Seq Instruction) }
