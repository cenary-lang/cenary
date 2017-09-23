{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Codegen.Types where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Identity
import qualified Data.Text as T

data CodegenError =
  MeaninglessExpr
  deriving Show

newtype Evm a = Evm { runEvm :: StateT CodegenState (WriterT T.Text (Either CodegenError)) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadError CodegenError, MonadWriter T.Text)

data CodegenState = CodegenState
  { _byteCode   :: !T.Text
  , _memPointer :: !Integer
  }

makeLenses ''CodegenState

initCodegenState :: CodegenState
initCodegenState = CodegenState
  { _byteCode = ""
  , _memPointer = -1
  }
