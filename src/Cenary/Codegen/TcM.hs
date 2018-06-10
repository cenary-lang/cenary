module Cenary.Codegen.TcM where

import           Cenary.Codegen.CodegenError
import           Cenary.Codegen.Evm
import           Cenary.Syntax
import           Control.Monad.Except

-- | Class of monads that can perform typechecking
class TcM m where
  checkTyEq :: Name -> PrimType -> PrimType -> m ()

instance TcM Evm where
  checkTyEq name tyL tyR =
    unless (tyL == tyR) $ throwError $ TypeMismatch name tyR tyL
