{-# LANGUAGE TemplateHaskell #-}

module Ivy.EvmAPI.Program where

import Ivy.EvmAPI.Instruction (Instruction (..))
import qualified Data.Sequence as Seq
import Control.Lens

-- | TODO: This should have its own module
newtype Program = Program { _unProgram :: (Seq.Seq Instruction) }

makeLenses ''Program
