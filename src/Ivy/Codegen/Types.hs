{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ivy.Codegen.Types where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens                   hiding (Context)
import           Control.Monad.Except
import           Control.Monad.Logger           hiding (logInfo)
import           Control.Monad.Logger.CallStack (logInfo)
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Functor                   (($>))
import qualified Data.Map                       as M
import           Data.Semigroup                 ((<>))
import qualified Data.Text                      as T
--------------------------------------------------------------------------------
import           Ivy.Syntax                     (PrimType(..))
--------------------------------------------------------------------------------

data CodegenError =
    VariableNotDeclared String
  | VariableAlreadyDeclared String
  | VariableNotDefined String
  | TypeMismatch String PrimType PrimType
  | ScopedTypeViolation String PrimType PrimType
  | InternalError String
  | WrongOperandTypes PrimType PrimType
  | NoReturnStatement

type Addr = Integer
data Operand = Operand PrimType Addr

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
  show (InternalError err) = "InternalError: " <> err
  show (WrongOperandTypes tyL tyR) = "Wrong operand types: "
                                   <> "Expected a value of "
                                   <> show tyL
                                   <> " but a value of type "
                                   <> show tyR
                                   <> " is provided."
  show NoReturnStatement = "Functions should have return statement as their last statement"

data Size =
    Size_1
  | Size_2
  | Size_4
  | Size_8
  | Size_32
  deriving (Show, Eq, Ord)

type Address = Integer
type Env = [Context] -- A stack
type Context = M.Map String (PrimType, Maybe Address)

data MemBlock = MemBlock
  { _memBlockIndex     :: Integer
  , _memBlockAllocated :: Integer
  } deriving Show

makeLenses ''MemBlock

type MemPointers = M.Map Size MemBlock

data CodegenState = CodegenState
  { _byteCode    :: !T.Text
  , _memPointers :: !MemPointers
  , _env         :: !Env
  , _memory      :: !(M.Map Integer Integer)
  , _pc          :: Integer
  }

makeLenses ''CodegenState

newtype Evm a = Evm { runEvm :: StateT CodegenState (LoggingT (ExceptT CodegenError IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CodegenState, MonadError CodegenError, MonadLogger)

type ScopeLevel = Int

data VariableStatus = NotDeclared
                    | Decl PrimType
                    | Def PrimType Integer
                    | Fun Integer
                    | Error CodegenError
