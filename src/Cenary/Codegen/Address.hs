module Cenary.Codegen.Address where

import           Cenary.Codegen.Scope (Scope)
data Address = VarAddr (Maybe Integer) Scope
             -- ^      Variable adress
             | FunAddr Integer
             -- ^      PC
             deriving Show

