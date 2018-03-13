{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Ivy.Main where

------------------------------------------------------
import           Control.Error.Util (hoistEither)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import           Data.Bifunctor (bimap)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (hClose)
import           System.Process hiding (env)
import           Text.Parsec (ParseError)
import           Text.Pretty.Simple (pPrint)
------------------------------------------------------
import qualified Ivy.Codegen as C
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types (CodegenError (..), CodegenState (..), Env,
                                    Sig (..), initProgram, runEvm)
import           Ivy.EvmAPI.Instruction (generateByteCode)
import           Ivy.Options (Mode (..), Options (..), parseOptions)
import qualified Ivy.Parser as P
import qualified Ivy.Syntax as S
import           Ivy.Utils.EvmAsm (asm)
------------------------------------------------------

data Error =
    Parsing ParseError
  | Codegen CodegenError

instance Show Error where
  show (Parsing err) = "Parsing Error: " <> show err
  show (Codegen err) = "Compile Error: " <> show err

codegen :: Monad m => CodegenState -> [S.FunStmt] -> ExceptT Error m T.Text
codegen initState functions =
  fmap (T.pack . generateByteCode . _program)
    $ hoistEither
    $ bimap Codegen id
    $ execStateT (runEvm (C.codegenPhases functions)) initState

type AST = [S.FunStmt]

parse :: MonadIO m => T.Text -> ExceptT Error m AST
parse code =
  case P.parse code of
    Left (Parsing -> err) -> throwError err
    Right result          -> return result

execByteCode :: MonadIO m => Environment -> T.Text -> m T.Text
execByteCode env byteCode = do
  hout <- get_hout
  liftIO $ T.hGetContents hout <* hClose hout
    where
      get_hout = case env of
        Testing -> do
          (_, _, Just hout, _) <- liftIO $ createProcess
            (proc "evm" ["--debug", "--code", T.unpack byteCode, "run"]) { std_err = CreatePipe }
          pure hout
        Console -> do
          (_, Just hout, _, _) <- liftIO $ createProcess
            (proc "evm" ["--debug", "--code", T.unpack byteCode, "run"]) { std_out = CreatePipe }
          pure hout

-- Given the address of the deployer, prepares the environment
initEnv :: Integer -> Env
initEnv _userAddr = (Sig "main" [] S.TInt, [M.empty])

initCodegenState :: CodegenState
initCodegenState =
  CodegenState
    { _memPointer = 0
    , _memory           = initMemory
    , _env              = initEnv 0 -- TODO: Address of the deployer comes here.
    , _pc               = 0
    , _funcRegistry     = M.empty
    , _program          = initProgram
    , _funcOffset       = 0
    }

data Environment =
    Console
  | Testing

main :: IO ()
main = do
  Options mode inputFile <- parseOptions
  code <- T.readFile inputFile
  result <- runExceptT (go code mode)
  case result of
    Left err -> T.putStrLn "" >> T.putStrLn (T.pack (show err))
    Right () -> return ()
  where
    go :: T.Text -> Mode -> ExceptT Error IO ()
    go code mode =
      case mode of
        Ast -> do
          ast <- parse code
          liftIO $ pPrint ast
        ByteCode ->
          parse code >>= codegen initCodegenState >>= liftIO . T.putStrLn
        Run ->
          void $ parse code >>= codegen initCodegenState >>= execByteCode Console
        Asm -> do
          let byteCode = asm (T.lines code)
          void $ liftIO (execByteCode Console byteCode)
        Disasm -> do
          parse code >>= codegen initCodegenState >>= liftIO . T.writeFile "yis"
          (_, Just hout, _, _) <- liftIO $ createProcess (proc "evm" ["disasm", "yis"]){ std_out = CreatePipe  }
          liftIO $ do
            T.putStrLn =<< T.hGetContents hout
            callProcess "rm" ["yis"]
            hClose hout
        Deploy -> do
          bytecode <- parse code >>= codegen initCodegenState
          liftIO $ callProcess "cp" ["deployment/deployment.js", "deployment/deployment.backup.js"]
          liftIO $ callProcess "sed" ["-i", "", "s/@bin@/" <> T.unpack bytecode <> "/", "deployment/deployment.js"]
        RewindDeploy -> do
          liftIO $ callProcess "rm" ["deployment/deployment.js"]
          liftIO $ callProcess "mv" ["deployment/deployment.backup.js", "deployment/deployment.js"]
