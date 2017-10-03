{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Logger
import           Control.Monad.Except
import           Data.Monoid          ((<>))
import           Data.Text            as T
import           Data.Text.IO         as T
import           System.Environment
import           System.Process
import           System.IO            (hClose)
import           Text.Parsec          (ParseError)
------------------------------------------------------
import qualified Ivy.Codegen         as C
import           Ivy.Codegen.Types   (CodegenState (..), initCodegenState,
                                       runEvm, CodegenError(..))
import qualified Ivy.Parser          as P
import qualified Ivy.Syntax          as S
import           Options             (Options (..), Mode (..), parseOptions, debug)
import           Utils.EvmAsm        (asm)
------------------------------------------------------

data Error =
    Parsing ParseError
  | Codegen CodegenError
  | RuntimeError T.Text

instance Show Error where
  show (Parsing err) = "Parsing Error: " <> show err
  show (Codegen err) = "Codegen Error: " <> show err
  show (RuntimeError err) = "Codegen Error: " <> T.unpack err

codegen :: Mode -> CodegenState -> [S.Expr] -> ExceptT Error IO T.Text
codegen _ _ [] = return ""
codegen mode state (e:ex) = do
  result <- liftIO $ runExceptT (runStdoutLoggingT (execStateT (runEvm (C.codegenTop' e)) state))
  case result of
    Left (Codegen -> err)       -> throwError err
    Right newState ->
      (_byteCode newState <>) <$> codegen mode newState ex

parse :: T.Text -> ExceptT Error IO [S.Expr]
parse code =
  case P.parseTopLevel code of
    Left (Parsing -> err) -> throwError err
    Right result -> return result

execByteCode :: T.Text -> IO T.Text
execByteCode byteCode = do
  (_, Just hout, _, _) <- createProcess
    (proc "evm" ["--debug", "--code", T.unpack byteCode, "run"]) { std_out = CreatePipe }
  hGetContents hout <* hClose hout

main :: IO ()
main = do
  (Options mode inputFile) <- parseOptions
  print mode
  print inputFile

  code <- T.readFile inputFile
  result <- runExceptT (go code mode)
  case result of
    Left err -> T.putStrLn "" >> T.putStrLn (T.pack (show err))
    Right () -> return ()
  where
    go :: T.Text -> Mode -> ExceptT Error IO ()
    go code mode = 
      case mode of
        Run -> do
          ast <- parse code
          codegen mode initCodegenState ast >>= liftIO . execByteCode >>= liftIO . T.putStrLn
          liftIO $ print ast
        Debug ->
          void $ parse code >>= codegen mode initCodegenState
        Asm -> do
          let byteCode = asm (T.lines code)
          liftIO $ print $ "Bytecode: " <> byteCode
          liftIO $ T.putStrLn =<< execByteCode byteCode
        Disasm -> do
          parse code >>= codegen mode initCodegenState >>= liftIO . T.writeFile "yis"
          (_, Just hout, _, _) <- liftIO $ createProcess (proc "evm" ["disasm", "yis"]){ std_out = CreatePipe  }
          liftIO $ do
            T.putStrLn =<< hGetContents hout
            callProcess "rm" ["yis"]
            hClose hout
