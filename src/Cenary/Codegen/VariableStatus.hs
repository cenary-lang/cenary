{-# LANGUAGE GADTs #-}

module Cenary.Codegen.VariableStatus where

import           Cenary.Codegen.Scope
import           Cenary.Syntax

data VarVariableStatus
data FuncVariableStatus

data VariableStatus a where
  NotDeclared :: VariableStatus a
  Decl        :: PrimType -> Scope -> VariableStatus VarVariableStatus
  Def         :: PrimType -> Integer -> Scope -> VariableStatus VarVariableStatus
  FunDef      :: PrimType -> Integer -> VariableStatus FuncVariableStatus
