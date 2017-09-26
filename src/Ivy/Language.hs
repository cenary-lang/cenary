module Ivy.Language where

--------------------------------------------------------------------------------
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.Token
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

style :: LanguageDef state
style =
  emptyDef
    { commentLine     = "--"
    , reservedOpNames = ops
    , reservedNames   = names
    }

ops :: [String]
ops =
  [ "+"
  , "*"
  , "-"
  , "/"
  , ";"
  , "."
  ]

names :: [String]
names =
  ["if","if", "then", "else", "times", "endtimes", "do"]
