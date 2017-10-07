{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Ivy.Codegen.Types where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map              as M
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
--------------------------------------------------------------------------------
import           Ivy.Syntax            (PrimType(..))
--------------------------------------------------------------------------------

data CodegenError =
    VariableNotDeclared String
  | VariableAlreadyDeclared String
  | VariableNotDefined String
  | TypeMismatch String PrimType PrimType
  | ScopedTypeViolation String PrimType PrimType
  | InternalError String
  | WrongOperandTypes PrimType PrimType

type Addr = Integer
data Operand = Operand PrimType Addr
type Size = Integer -- In bytes

instance Show CodegenError where
  show (VariableNotDeclared var) = "Variable " <> var <> " is not declared."
  show (VariableNotDefined var)  = "Variable " <> var <> " is not defined."
  show (VariableAlreadyDeclared var) = "Variable " <> var <> " is already declared."
  show (TypeMismatch name expected actual) = "Type mismatch for variable "
                                          <> name
                                          <> ". Expected: "
                                          <> show expected
                                          <> " , actual: "
                                          <> show actual
  show (ScopedTypeViolation name global local) = "TypeScopeViolation for variable "
                                              <> name
                                              <> ". In global scope, it has "
                                              <> show global
                                              <> " while in local scope it has "
                                              <> show local

newtype Evm a = Evm { runEvm :: StateT CodegenState (LoggingT (ExceptT CodegenError IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CodegenState, MonadError CodegenError, MonadLogger)

type ScopeLevel = Int
type Address = Integer

data Scope = Local | Global
data VariableStatus = NotDeclared
                    | Decl PrimType Scope
                    | Def PrimType Scope Integer
                    | Error CodegenError

type SymbolTable = M.Map String (PrimType, Maybe Address)

data CodegenState = CodegenState
  { _byteCode    :: !T.Text
  , _memPointer  :: !Integer
  , _globalScope :: !SymbolTable
  , _localScope  :: !SymbolTable
  }

makeLenses ''CodegenState

sizeof :: PrimType -> Integer
sizeof IntT = 32
sizeof CharT = 1
sizeof (Array length ty) = length * sizeof ty

initCodegenState :: CodegenState
initCodegenState = CodegenState
  { _byteCode   = ""
  , _memPointer = 0
  , _globalScope = M.empty
  , _localScope = M.empty
  }
