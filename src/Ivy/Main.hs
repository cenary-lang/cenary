{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

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
import qualified Data.Text.Lazy as TL
import           System.Exit (ExitCode (..), exitWith)
import           System.IO (hClose)
import           System.Process hiding (env)
import           Text.Parsec (ParseError)
import           Text.Pretty.Simple (pPrint)
------------------------------------------------------
import qualified Evm.Abi as Abi
import qualified Ivy.Codegen as C
import           Ivy.Codegen.Memory
import           Ivy.Codegen.Types (CodegenError (..), CodegenState (..), Env,
                                    Sig (..), initProgram, runEvm)
import           Ivy.EvmAPI.API (generateByteCode)
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

astToAbi :: forall m. MonadError Error m => AST -> m Abi.Abi
astToAbi = fmap Abi.Abi . mapM func_to_abi
  where
    func_to_abi :: S.FunStmt -> m Abi.Function
    func_to_abi (S.FunStmt (S.FunSig _ name args) _ retTy) = do
      inputs <- mapM arg_to_abi_input args
      abi_ret_ty <- to_abi_ty retTy
      pure $ Abi.Function
        { Abi._functionName = name
        , Abi._functionType = Abi.FunctionTypeFunction
        , Abi._functionConstant = True
        , Abi._functionPayable = False
        , Abi._functionInputs = inputs
        , Abi._functionOutputs = [ Abi.Output "output_name" abi_ret_ty ]
        }

    arg_to_abi_input :: (S.PrimType, S.Name) -> m Abi.Input
    arg_to_abi_input (ty, name) = Abi.Input name <$> (to_abi_ty ty)

    to_abi_ty :: S.PrimType -> m Abi.AbiType
    to_abi_ty = \case
      S.TInt -> pure Abi.AbiTy_uint256
      S.TChar -> pure Abi.AbiTy_char
      S.TBool -> pure Abi.AbiTy_bool
      ty -> throwError $ Codegen $ InternalError $ "Don't know how to convert type " <> show ty <> " to abi representation"

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
          ast <- parse code
          abi <- Abi.encodeAbi <$> astToAbi ast
          bytecode <- codegen initCodegenState ast
          liftIO $ callProcess "cp" ["deployment/deployment.js", "deployment/deployment.current.js"]
          liftIO $ callProcess "sed" ["-i", "", "s/@bin@/" <> T.unpack bytecode <> "/", "deployment/deployment.current.js"]
          liftIO $ callProcess "sed" ["-i", "", "s/@abi@/" <> TL.unpack abi <> "/g", "deployment/deployment.current.js"]
        RewindDeploy -> do
          liftIO $ callProcess "rm" ["deployment/deployment.current.js"]
