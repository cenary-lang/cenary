{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ivy.Codegen.Memory where

--------------------------------------------------------------------------------
import           Control.Lens hiding (ix, op)
import           Control.Monad
import           Data.Function (on)
import           Data.List (find, groupBy)
import qualified Data.Map as M
import           Data.Monoid
--------------------------------------------------------------------------------
import           Ivy.Codegen.Types
import           Ivy.EvmAPI.API
import           Ivy.Syntax
--------------------------------------------------------------------------------
