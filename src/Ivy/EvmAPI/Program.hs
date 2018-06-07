{-# LANGUAGE TemplateHaskell #-}

module Ivy.EvmAPI.Program where

import           Control.Lens
import qualified Data.Sequence as Seq
import           Ivy.EvmAPI.Instruction (Instruction (..))

-- | TODO: This should have its own module
newtype Program = Program { _unProgram :: (Seq.Seq Instruction) }

makeLenses ''Program
