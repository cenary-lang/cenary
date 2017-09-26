{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Ivy.Codegen.Types where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Logger
import           Data.Functor.Identity
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Semigroup        ((<>))
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data CodegenError =
    VariableNotDeclared String
  | VariableNotDefined String
  | InternalError String

instance Show CodegenError where
  show (VariableNotDeclared var) = "Variable " <> var <> " is not declared."
  show (VariableNotDefined var) = "Variable " <> var <> " is not defined."

newtype Evm a = Evm { runEvm :: StateT CodegenState (LoggingT (ExceptT CodegenError IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CodegenState, MonadError CodegenError, MonadLogger)

type ScopeLevel = Int

type SymbolTable = M.Map String (Maybe Integer) -- Symbols to addresses

data CodegenState = CodegenState
  { _byteCode   :: !T.Text
  , _memPointer :: !Integer
  , _symTables   :: !(M.Map ScopeLevel SymbolTable)
  }

makeLenses ''CodegenState

initCodegenState :: CodegenState
initCodegenState = CodegenState
  { _byteCode   = ""
  , _memPointer = -32
  , _symTables  = M.fromList [(0, M.empty)]
  }
