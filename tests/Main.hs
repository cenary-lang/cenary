{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import System.Process
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Cenary.Main as Cenary
import Control.Monad.Except (runExceptT)
import Data.Semigroup ((<>))
import Control.Concurrent
import System.IO
import Cenary.Deployment (rewindDeployment)

data ITestSource = ITestSource
  { _filename :: String
  , _assertion :: T.Text
  , _functionCall :: String
  }

newtype ITest = ITest [ITestSource]

tests :: ITest
tests = ITest $
  [ ITestSource "tests/sources/Adder.cen" "7" "add(3, 4)"
  ]

runTests :: ITest -> IO ()
runTests (ITest sources) = withTestrpc $ traverse_ runTest sources

withTestrpc :: IO a -> IO a
withTestrpc action = do
  let cp = (proc "testrpc" ["--gasLimit", "90000000", "--gasPrice", "1"]) { std_out = CreatePipe }
  withCreateProcess cp $ \_ _ _ _ -> threadDelay 3000000 >> action

runTest :: ITestSource -> IO ()
runTest (ITestSource fn as fc) = do
  content <- T.readFile ("tests/sources/" <> fn)
  _ <- runExceptT $ Cenary.deploy content
  callProcess "sed" ["-i", "", "s/@call@/" <> fc <> "/", "deployment/deployment.current.js"]
  (_, Just hout, _, _) <- createProcess (proc "node" ["deployment/deployment.current.js"]) { std_out = CreatePipe }
  result <- hGetContents hout
  (T.strip (T.pack result)) `shouldBe` as
  rewindDeployment

main :: IO ()
main = withTestrpc $ hspec $ do
  describe "Prelude.head" $ do
    it "Adds two numbers" $ do
      runTest $ ITestSource "Adder.cen" "5" "add(1, 4)"
      runTest $ ITestSource "Adder.cen" "3" "add(1, 2)"
    it "Can branch properly" $ do
      runTest $ ITestSource "Branching.cen" "3" "main(4)"
      runTest $ ITestSource "Branching.cen" "6" "main(5)"
    it "Access and modify hashmaps" $ do
      runTest $ ITestSource "Hashmaps.cen" "9" "string_keys()"
      runTest $ ITestSource "Hashmaps.cen" "28" "integer_keys()"
    it "Can resize arrays" $ do
       runTest $ ITestSource "Resizing.cen" "5" "read()"
       runTest $ ITestSource "Resizing.cen" "98" "write(98)"
       runTest $ ITestSource "Resizing.cen" "0" "out_of_bounds_read()"
       runTest $ ITestSource "Resizing.cen" "0" "out_of_bounds_write()"
    it "Can run identity function" $ do
       runTest $ ITestSource "Id.cen" "3" "id(3)"
    it "Can compute 15th fibonacci number" $ do
       runTest $ ITestSource "Fibonacci.cen" "610" "fib(15)"
