{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Except
import           Data.Foldable (traverse_)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Ivy.Main as Ivy
import           System.FilePath
import           Test.Hspec

data Assertion =
    ShouldNotParse
  | ShouldParse CodegenAssertion

data CodegenAssertion =
    ShouldCodegen
  | ShouldNotCodegen

type SourceFile = FilePath

assertions :: [(Assertion, SourceFile)]
assertions =
  [ (ShouldParse ShouldNotCodegen, "Corrupted.ivy")
  , (ShouldParse ShouldCodegen, "FunCall.ivy")
  ]

main :: IO ()
main = hspec $ do
  traverse_ assert assertions

failOnLeft :: (a -> String) -> ExceptT a IO b -> IO b
failOnLeft onLeft action = runExceptT action >>= \case
  Left a -> expectationFailure (onLeft a) >> error "" -- assertFailure still returns () ...
  Right v -> pure v

failOnLeft_ :: String -> ExceptT a IO b -> IO b
failOnLeft_ = failOnLeft . const

assert :: (Assertion, SourceFile) -> SpecWith ()
assert (assertion, sourceFile) =
  describe sourceFile $
    case assertion of
      ShouldNotParse ->
        it "should not parse" $ shouldNotParse sourceFile
      ShouldParse ShouldNotCodegen -> do
        it "should parse, but should not generate code" $ do
          ast <- failOnLeft (("Source could not be parsed: " <>) . show) (parseSource sourceFile)
          shouldNotCodegen ast
      ShouldParse ShouldCodegen -> do
        it "should parse and generate code" $ do
          runExceptT (parseSource sourceFile) >>= \case
            Left err -> expectationFailure $ "Source could not be parsed: " <> show err
            Right ast -> do
              _ <- failOnLeft
                (("Code could not be generated for the source: " <>) . show)
                (Ivy.codegen Ivy.initCodegenState ast)
              pure ()

getSource :: FilePath -> IO T.Text
getSource filename =
  T.pack <$> readFile ("tests" </> "sources" </> filename)

shouldNotParse :: SourceFile -> IO ()
shouldNotParse sourceFile = do
  runExceptT (parseSource sourceFile) >>= \case
    Left _  -> pure ()
    Right ast -> expectationFailure $ "Source could unexpectedly be parsed: " <> show ast

parseSource :: SourceFile -> ExceptT Ivy.Error IO Ivy.AST
parseSource sourceFile = Ivy.parse =<< (liftIO (getSource sourceFile))

shouldNotCodegen :: Ivy.AST -> IO ()
shouldNotCodegen ast =
  runExceptT (Ivy.codegen Ivy.initCodegenState ast) >>= \case
    Left _ -> pure ()
    Right _ -> expectationFailure $ "Code is generated, it should not been generated." -- Not showing error because it's just a big bytecode.
