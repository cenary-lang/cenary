{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------
import           Data.Monoid  ((<>))
import           Data.Text.IO as T
import           Control.Monad.Writer
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
      let result = runWriterT (C.codegen expr)
      case result of
        Left err       -> print $ "Codegen error: " <> show err
        Right (byteCode, logs) -> do
          T.putStrLn "=== LOGS ==="
          T.putStrLn logs
          T.putStrLn "============"
          T.putStrLn "=== RESULT ==="
          T.putStrLn byteCode
          T.putStrLn "============"
