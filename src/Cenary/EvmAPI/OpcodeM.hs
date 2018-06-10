module Cenary.EvmAPI.OpcodeM where

import Cenary.EvmAPI.Instruction
import Cenary.EvmAPI.Program
import Cenary.Codegen.Evm
import Cenary.Codegen.CodegenState
import Control.Lens

-- | Class of monads that can run opcodes
class Monad m => OpcodeM m where
  op :: Instruction -> m ()

instance OpcodeM Evm where
  op instr = do
    let (_, cost) = toOpcode instr
    pc += cost
    program %= (addInstr instr)

