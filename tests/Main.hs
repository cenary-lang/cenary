{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Except
import           Data.Foldable (traverse_)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Cenary.Main as Cenary
import           System.FilePath
import           Test.Hspec

data Assertion =
    ShouldNotParse
  | ShouldParse CodegenAssertion

data CodegenAssertion =
    ShouldNotCodegen
  | ShouldCodegen (T.Text -> Bool) -- Should generate code and output matches the given assertion

type SourceFile = FilePath

assertions :: [(String, Assertion, SourceFile)]
assertions =
  [ ( "Should not parse source with corrupted syntax"
    , ShouldParse ShouldNotCodegen
    , "Corrupted.cenary"
    )
  , ( "Should be able to generate code for if statements"
    , ShouldParse (ShouldCodegen (containsChars ['y', 'i', 'g', 'i', 't']))
    , "FunCall.cenary"
    )
    -- Computes 15th element of fibonacci series. 0x0262 = 610d
  , ( "Should calculate 15th element of Fibonacci series as 610"
    , ShouldParse (ShouldCodegen (T.isInfixOf "02 62"))
    , "Fibonacci.cenary"
    )
  , ("Should NOT calculate 15th element of Fibonacci series as 611"
    , ShouldParse (ShouldCodegen (not . T.isInfixOf "02 63"))
    , "Fibonacci.cenary"
    )
  ]

containsChars :: [Char] -> T.Text -> Bool
containsChars cx t = all (\c -> T.isInfixOf (T.singleton c) t) cx

main :: IO ()
main = hspec $ do
  traverse_ assert assertions

assert :: (String, Assertion, SourceFile) -> SpecWith ()
assert (description, assertion, sourceFile) =
  describe sourceFile $
    case assertion of
      ShouldNotParse ->
        it description $ shouldNotParse sourceFile
      ShouldParse ShouldNotCodegen -> do
        it description $ do
          ast <- shouldParse sourceFile
          shouldNotCodegen ast
      ShouldParse (ShouldCodegen outputAssertion) -> do
        it description $ do
          ast <- shouldParse sourceFile
          output <- Cenary.execByteCode Cenary.Testing =<< shouldCodegen ast
          outputAssertion output `shouldBe` True
          pure ()

getSource :: FilePath -> IO T.Text
getSource filename =
  T.pack <$> readFile ("tests" </> "sources" </> filename)

shouldNotParse :: SourceFile -> IO ()
shouldNotParse sourceFile = do
  runExceptT (parseSource sourceFile) >>= \case
    Left _  -> pure ()
    Right ast -> expectationFailure $ "Source could unexpectedly be parsed: " <> show ast

failOnLeft :: (a -> String) -> ExceptT a IO b -> IO b
failOnLeft onLeft action = runExceptT action >>= \case
  Left a -> expectationFailure (onLeft a) >> error "" -- assertFailure still returns () ...
  Right v -> pure v

failOnLeft_ :: String -> ExceptT a IO b -> IO b
failOnLeft_ = failOnLeft . const

shouldParse :: SourceFile -> IO Cenary.AST
shouldParse sourceFile = failOnLeft (("Source could not be parsed: " <>) . show) (parseSource sourceFile)

shouldCodegen :: Cenary.AST -> IO T.Text
shouldCodegen ast = 
  failOnLeft
    (("Code could not be generated for the source: " <>) . show)
    (Cenary.codegen Cenary.initCodegenState ast)

parseSource :: SourceFile -> ExceptT Cenary.Error IO Cenary.AST
parseSource sourceFile = Cenary.parse =<< (liftIO (getSource sourceFile))

shouldNotCodegen :: Cenary.AST -> IO ()
shouldNotCodegen ast =
  runExceptT (Cenary.codegen Cenary.initCodegenState ast) >>= \case
    Left _ -> pure ()
    Right _ -> expectationFailure $ "Code is generated, it should not been generated." -- Not showing error because it's just a big bytecode.
