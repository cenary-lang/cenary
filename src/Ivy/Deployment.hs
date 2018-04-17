module Ivy.Deployment where

import           Control.Monad.IO.Class
import           Data.Semigroup ((<>))
import           System.Process hiding (env)

prepareDeployment
  :: Monad m
  => code
  -> (code -> m ast)
  -> (ast -> m abi)
  -> (ast -> m bytecode)
  -> m (bytecode, abi)
prepareDeployment code parse toAbi toBytecode = do
  ast <- parse code
  abi <- toAbi ast
  bytecode <- toBytecode ast
  pure (bytecode, abi)

runDeployment
  :: String
  -> String
  -> IO ()
runDeployment bytecode abi = do
  liftIO $ callProcess "cp" ["deployment/deployment.js", "deployment/deployment.current.js"]
  liftIO $ callProcess "sed" ["-i", "", "s/@bin@/" <> bytecode <> "/", "deployment/deployment.current.js"]
  liftIO $ callProcess "sed" ["-i", "", "s/@abi@/" <> abi <> "/g", "deployment/deployment.current.js"]

rewindDeployment :: IO ()
rewindDeployment =
  callProcess "rm" ["deployment/deployment.current.js"]
