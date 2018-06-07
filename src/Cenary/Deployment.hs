module Cenary.Deployment where

import           Control.Applicative (liftA2)
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
  liftA2 (,) (toBytecode ast) (toAbi ast)

runDeployment
  :: String
  -> String
  -> IO ()
runDeployment bytecode abi = do
  callProcess "cp" ["deployment/deployment.js", "deployment/deployment.current.js"]
  callProcess "sed" ["-i", "", "s/@bin@/" <> bytecode <> "/", "deployment/deployment.current.js"]
  callProcess "sed" ["-i", "", "s/@abi@/" <> abi <> "/g", "deployment/deployment.current.js"]

rewindDeployment :: IO ()
rewindDeployment =
  callProcess "rm" ["deployment/deployment.current.js"]
