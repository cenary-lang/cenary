{-# LANGUAGE TemplateHaskell #-}

module Cenary.EvmAPI.Program where

import           Control.Lens
import qualified Data.Sequence as Seq
import           Cenary.EvmAPI.Instruction (Instruction (..))

-- | TODO: This should have its own module
newtype Program = Program { _unProgram :: (Seq.Seq Instruction) }

makeLenses ''Program

initProgram :: Program
initProgram = Program Seq.empty

addInstr :: Instruction -> Program -> Program
addInstr instr p = p & unProgram %~ (instr <|)
