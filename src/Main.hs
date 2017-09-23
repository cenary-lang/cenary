{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------
import           Text.Parsec
------------------------------------------------------
import qualified EvmL.Parser as P
------------------------------------------------------

main :: IO ()
main =
  case parse P.expr "<unknown>" "if rue then false" of
    Left err -> print err
    Right expr  -> print expr

