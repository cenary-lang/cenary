{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Logger
import           Control.Monad.Except
import           Data.Monoid          ((<>))
import           Control.Arrow        (first, second)
import           Data.Text            as T
import           Data.Text.IO         as T
import qualified Data.Map as M
import           System.Environment
import           System.Process
import           Control.Error.Util (hoistEither)
import           System.IO            (hClose)
import           Text.Parsec          (ParseError)
import           Text.Pretty.Simple   (pPrint)
------------------------------------------------------
import qualified Ivy.Codegen         as C
import           Ivy.Codegen.Types   (CodegenState (..),
                                       runEvm, CodegenError(..), Env, Sig (..))
import qualified Ivy.Parser          as P
import qualified Ivy.Syntax          as S
import           Options             (Options (..), Mode (..), parseOptions)
import           Utils.EvmAsm        (asm)
import           Ivy.Codegen.Memory
------------------------------------------------------

data Error =
    Parsing ParseError
  | Codegen CodegenError

instance Show Error where
  show (Parsing err) = "Parsing Error: " <> show err
  show (Codegen err) = "Codegen Error: " <> show err

-- | These 3 functions should be refactored with StateT monad transformer.
codegenFundef :: CodegenState -> [S.SFunDef] -> Either Error CodegenState
codegenFundef initState stmts = do
  go initState stmts
    where
      go :: CodegenState -> [S.SFunDef] -> Either Error CodegenState
      go state [] = Right state
      go state (stmt:xs) =
        case execStateT (runEvm (C.codegenFunDef stmt)) state of
          Left (Codegen -> err) -> throwError err
          Right newState -> go newState xs

codegen :: Monad m => CodegenState -> [S.AnyStmt] -> ExceptT Error m T.Text
codegen state = hoistEither . pureCodegen state

pureCodegen :: CodegenState -> [S.AnyStmt] -> Either Error T.Text
pureCodegen initState anyStmts = do
  let (stmts, fundefStmts) = splitStmts anyStmts
  stateAfterFunctions <- codegenFundef initState fundefStmts
  codegen' (stateAfterFunctions, stmts)
  where
    splitStmts :: [S.AnyStmt] -> ([S.Stmt], [S.SFunDef])
    splitStmts []                       = ([], [])
    splitStmts (S.FundefStmt fundef:xs) = second (fundef:) (splitStmts xs)
    splitStmts (S.Stmt stmt:xs)         = first (stmt:) (splitStmts xs)

codegen' :: (CodegenState, [S.Stmt]) -> Either Error T.Text
codegen' (_, []) = return ""
codegen' (state, e:ex) = do
  let result = execStateT (runEvm (C.codegenStmt e)) state
  case result of
    Left (Codegen -> err)       -> throwError err
    Right newState ->
      (_byteCode newState <>) <$> codegen' (newState, ex)

parse :: MonadIO m => T.Text -> ExceptT Error m [S.AnyStmt]
parse code =
  case P.parseTopLevel code of
    Left (Parsing -> err) -> throwError err
    Right result -> return result

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
    { _byteCode         = ""
    , _memPointers      = initMemPointers
    , _memory           = initMemory
    , _env              = initEnv 0 -- TODO: Address of the deployer comes here.
    , _pc               = 0
    , _funcRegistry     = M.empty
    }

main :: IO ()
main = do
  (Options mode inputFile) <- parseOptions
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
