{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Monoid          ((<>))
import           Data.Text            as T
import           Data.Text.IO         as T
import           System.Environment
import           System.Process
------------------------------------------------------
import qualified EvmL.Codegen         as C
import           EvmL.Codegen.Types   (CodegenState (..), initCodegenState,
                                       runEvm)
import qualified EvmL.Parser          as P
import qualified EvmL.Syntax          as S
import           Options              (Options (..), Mode (..), parseOptions, debug)
------------------------------------------------------

codegenTop :: Mode -> [S.Expr] -> CodegenState -> IO T.Text
codegenTop _ [] _ = return ""
codegenTop mode (e:ex) state = do
  when (debug mode) $ Prelude.putStrLn $ "[E] " <> show e
  let result = runWriterT (execStateT (runEvm (C.codegenTop e)) state)
  case result of
    Left err       -> "" <$ print ("Codegen error: " <> show err)
    Right (newState, logs) -> do
      when (debug mode) $ do
        T.putStrLn $ "[B] " <> _byteCode newState
        T.putStrLn $ "[S] " <> T.pack (show (_symTable newState))
        T.putStrLn ""
      (_byteCode newState <>) <$> codegenTop mode ex newState

main :: IO ()
main = do
  (Options mode inputFile) <- parseOptions
  code <- T.readFile inputFile
  case P.parseTopLevel code of
    Left err    -> print err
    Right exprs -> do
      byteCode <- codegenTop mode exprs initCodegenState
      case mode of
        Run -> do
          (_, Just hout, _, _) <- createProcess (proc "evm" ["--debug", "--code", T.unpack byteCode, "run"]){ std_out = CreatePipe }
          T.putStrLn =<< hGetContents hout
        Disasm -> do
          T.writeFile "yis" byteCode 
          (_, Just hout, _, _) <- createProcess (proc "evm" ["disasm", "yis"]){ std_out = CreatePipe  }
          T.putStrLn =<< hGetContents hout
          callProcess "rm" ["yis"]
          
