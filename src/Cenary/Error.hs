module Cenary.Error where

import           Data.Semigroup ((<>))
import           Cenary.Codegen.CodegenError
import           Text.Parsec (ParseError)

data Error =
    Parsing ParseError
  | Codegen CodegenError

instance Show Error where
  show (Parsing err) = "Parsing Error: " <> show err
  show (Codegen err) = "Compile Error: " <> show err
