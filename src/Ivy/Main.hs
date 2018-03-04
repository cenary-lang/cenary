{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Ivy.Main (main, parse, codegen) where

------------------------------------------------------
import           Control.Error.Util (hoistEither)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Text as T
import           Data.Text.IO as T
import           System.IO (hClose)
import           System.Process
import           Text.Parsec (ParseError)
import           Text.Pretty.Simple (pPrint)
------------------------------------------------------
import qualified Ivy.Codegen as C
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types (CodegenError (..), CodegenState (..), Env,
                                    Sig (..), initProgram, runEvm)
import           Ivy.EvmAPI.Instruction (generateByteCode)
import qualified Ivy.Parser as P
import qualified Ivy.Syntax as S
import           Ivy.Options (Mode (..), Options (..), parseOptions)
import           Ivy.Utils.EvmAsm (asm)
------------------------------------------------------

data Error =
    Parsing ParseError
  | Codegen CodegenError

instance Show Error where
  show (Parsing err) = "Parsing Error: " <> show err
  show (Codegen err) = "Compile Error: " <> show err

codegenFundef :: CodegenState -> [S.FunStmt] -> Either Error CodegenState
codegenFundef initState stmts = do
  case execStateT (runEvm action) initState of
    Left (Codegen -> err) -> throwError err
    Right state           -> return state
  where
    action = do
      mapM_ C.codegenFunDef stmts
      C.codegenFunCall "main" []

codegen :: Monad m => CodegenState -> [S.FunStmt] -> ExceptT Error m T.Text
codegen state = fmap (T.pack . generateByteCode . _program) . hoistEither . codegenFundef state

parse :: MonadIO m => T.Text -> ExceptT Error m [S.FunStmt]
parse code =
  case P.parse code of
    Left (Parsing -> err) -> throwError err
    Right result          -> return result

execByteCode :: MonadIO m => T.Text -> m ()
execByteCode byteCode = do
  (_, Just hout, _, _) <- liftIO $ createProcess
    (proc "evm" ["--debug", "--code", T.unpack byteCode, "run"]) { std_out = CreatePipe }
  void $ liftIO (hGetContents hout) <* liftIO (hClose hout)

-- Given the address of the deployer, prepares the environment
initEnv :: Integer -> Env
initEnv _userAddr = (Sig "main" [] S.TInt, [M.empty])

initCodegenState :: CodegenState
initCodegenState =
  CodegenState
    { _memPointers      = initMemPointers
    , _memory           = initMemory
    , _env              = initEnv 0 -- TODO: Address of the deployer comes here.
    , _pc               = 0
    , _funcRegistry     = M.empty
    , _program          = initProgram
    }

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
          parse code >>= codegen initCodegenState >>= liftIO . print
        Run ->
          parse code >>= codegen initCodegenState >>= execByteCode
        Asm -> do
          let byteCode = asm (T.lines code)
          liftIO (execByteCode byteCode)
        Disasm -> do
          parse code >>= codegen initCodegenState >>= liftIO . T.writeFile "yis"
          (_, Just hout, _, _) <- liftIO $ createProcess (proc "evm" ["disasm", "yis"]){ std_out = CreatePipe  }
          liftIO $ do
            T.putStrLn =<< hGetContents hout
            callProcess "rm" ["yis"]
            hClose hout
