{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------
import           Data.Monoid ((<>))
------------------------------------------------------
import qualified EvmL.Parser as P
import qualified EvmL.Codegen as C
------------------------------------------------------

main :: IO ()
main =
  case P.parse "if true then false" of
    Left err -> print err
    Right expr -> case C.codegen expr of
      Left err       -> print $ "Codegen error: " <> err
      Right byteCode -> print $ "Compile successful. Bytecode: " <> byteCode
