{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Monoid          ((<>))
import           Data.Text.IO         as T
------------------------------------------------------
import qualified EvmL.Codegen         as C
import           EvmL.Codegen.Types   (CodegenState(..), runEvm, initCodegenState)
import qualified EvmL.Parser          as P
import qualified EvmL.Syntax          as S
------------------------------------------------------

codegenTop :: [S.Expr] -> CodegenState -> IO ()
codegenTop [] _ = return ()
codegenTop (e:ex) state = do
  Prelude.putStrLn $ "[E] " <> show e
  let result = runWriterT (execStateT (runEvm (C.codegenTop e)) state)
  case result of
    Left err       -> print $ "Codegen error: " <> show err
    Right (newState, logs) -> do
      -- T.putStrLn "=== LOGS ==="
      -- T.putStrLn logs
      -- T.putStrLn "============"
      -- T.putStrLn "=== RESULT ==="
      T.putStrLn $ "[B] " <> _byteCode newState
      T.putStrLn ""
      -- T.putStrLn "============"
      codegenTop ex newState

main :: IO ()
main = do
  code <- T.readFile "stdlib.el"
  case P.parseTopLevel code of
    Left err -> print err
    Right exprs -> do
      codegenTop exprs initCodegenState
