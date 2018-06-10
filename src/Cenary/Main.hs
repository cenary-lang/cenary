{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Cenary.Main where

------------------------------------------------------
import           Control.Error.Util (hoistEither)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import           Data.Bifunctor (bimap)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           System.Exit (ExitCode (..), exitWith)
import           System.IO (hClose)
import           System.Process hiding (env)
import           Text.Pretty.Simple (pPrint)
------------------------------------------------------
import qualified Cenary.Codegen as C
import qualified Cenary.Codegen.MappingOrder as MO
import qualified Cenary.Codegen.Context as Ctx
import           Cenary.Codegen.Types (Address, CodegenState (..), Env (..),
                                       FuncRegistry (..), Sig (..), runEvm)
import           Cenary.Deployment (prepareDeployment, rewindDeployment,
                                    runDeployment)
import           Cenary.Error
import qualified Cenary.EvmAPI.AbiBridge as AbiBridge
import           Cenary.EvmAPI.API (generateByteCode)
import           Cenary.EvmAPI.Program (initProgram)
import           Cenary.Options (Mode (..), Options (..), parseOptions)
import qualified Cenary.Parser as P
import qualified Cenary.Syntax as S
import           Cenary.Utils.EvmAsm (asm)
------------------------------------------------------

codegen :: Monad m => CodegenState -> S.AST -> ExceptT Error m T.Text
codegen initState functions =
  fmap (T.pack . generateByteCode . _program)
    $ hoistEither
    $ bimap Codegen id
    $ execStateT (runEvm (C.codegenPhases functions)) initState

parse :: T.Text -> ExceptT Error IO S.AST
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
initEnv :: Integer -> Env Address
initEnv _userAddr = Env
  { _sig      = Sig "main" [] S.TInt
  , _contexts = [Ctx.newEmptyCtx]
  }

initCodegenState :: CodegenState
initCodegenState =
  CodegenState
    { _heapSpaceBegin   = 0
    , _sp               = 0
    , _framePtrs        = [0]
    , _stackStorageEnd  = 0
    , _env              = initEnv 0 -- TODO: Address of the deployer comes here.
    , _pc               = 0
    , _maxStackSize     = 0
    , _funcRegistry     = FuncRegistry M.empty
    , _program          = initProgram
    , _funcOffset       = 0
    , _mappingOrder     = MO.empty
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
    Left err -> T.putStrLn "" >> T.putStrLn (T.pack (show err)) >> exitWith (ExitFailure 1)
    Right () -> exitWith ExitSuccess
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
          (bytecode, abi) <- prepareDeployment
                               code
                               parse
                               AbiBridge.astToAbi
                               (codegen initCodegenState)
          liftIO $ runDeployment (T.unpack bytecode) (TL.unpack abi)
        RewindDeploy -> do
          liftIO rewindDeployment
