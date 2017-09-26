{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Logger
import           Control.Monad.Except
import           Data.Monoid          ((<>))
import           Data.Text            as T
import           Data.Text.IO         as T
import           System.Environment
import           System.Process
------------------------------------------------------
import qualified Ivy.Codegen         as C
import           Ivy.Codegen.Types   (CodegenState (..), initCodegenState,
                                       runEvm)
import qualified Ivy.Parser          as P
import qualified Ivy.Syntax          as S
import           Options              (Options (..), Mode (..), parseOptions, debug)
import           Utils.EvmAsm        (asm)
------------------------------------------------------

codegen :: Mode -> CodegenState -> [S.Expr] -> IO T.Text
codegen _ _ [] = return ""
codegen mode state (e:ex) = do
  when (debug mode) $ do
    T.putStrLn $ "[E] " <> T.pack (show e)
    T.putStrLn $ "[B] " <> _byteCode state
    T.putStrLn $ "[S] " <> T.pack (show (_symTables state))

  result <- runExceptT (runStdoutLoggingT (execStateT (runEvm (C.codegenTop 0 e)) state))
  case result of
    Left err       -> "" <$ print ("Codegen error: " <> show err)
    Right newState ->
      (_byteCode newState <>) <$> codegen mode newState ex

parse :: T.Text -> IO [S.Expr]
parse code =
  case P.parseTopLevel code of
    Left err -> error $ show err
    Right result -> return result

main :: IO ()
main = do
  (Options mode inputFile) <- parseOptions
  print mode
  print inputFile

  code <- T.readFile inputFile
  case mode of
    Run -> do
      ast <- parse code
      codegen mode initCodegenState ast >>= execByteCode
      print ast
    Debug ->
      void $ parse code >>= codegen mode initCodegenState
    Asm -> do
      let byteCode = asm (T.lines code)
      print $ "Bytecode: " <> byteCode
      execByteCode byteCode
    Disasm -> do
      parse code >>= codegen mode initCodegenState >>= T.writeFile "yis"
      (_, Just hout, _, _) <- createProcess (proc "evm" ["disasm", "yis"]){ std_out = CreatePipe  }
      T.putStrLn =<< hGetContents hout
      callProcess "rm" ["yis"]
  where
    execByteCode :: T.Text -> IO ()
    execByteCode byteCode = do
      (_, Just hout, _, _) <- createProcess (proc "evm" ["--debug", "--code", T.unpack byteCode, "run"]){ std_out = CreatePipe }
      T.putStrLn =<< hGetContents hout
