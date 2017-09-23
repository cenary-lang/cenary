{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------
import           Data.Monoid  ((<>))
import           Data.Text.IO as T
------------------------------------------------------
import qualified EvmL.Codegen as C
import qualified EvmL.Parser  as P
------------------------------------------------------

main :: IO ()
main = do
  code <- T.readFile "stdlib.el"
  case P.parse code of
    Left err -> print err
    Right expr -> do
      print $ "Expr: " <> show expr
      case C.codegen expr of
        Left err       -> print $ "Codegen error: " <> err
        Right byteCode -> print $ "Compile successful. Bytecode: " <> byteCode
