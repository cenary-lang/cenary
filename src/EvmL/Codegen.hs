{-# LANGUAGE OverloadedStrings #-}

module EvmL.Codegen where

--------------------------------------------------------------------------------
import qualified Data.Text   as T
--------------------------------------------------------------------------------
import           EvmL.Parser
--------------------------------------------------------------------------------

codegen :: Expr -> Either T.Text T.Text
codegen _ = Right "wow"
