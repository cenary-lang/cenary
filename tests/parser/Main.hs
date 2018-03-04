{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Except
import           Data.Foldable (traverse_)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Ivy.Main as Ivy
import           System.Directory
import           System.FilePath
import           Test.Hspec

data Assertion =
    ShouldParse
  | ShouldNotParse

main :: IO ()
main = hspec $ do
  describe "Corrupted.ivy" $ do
    it "should not parse" $ do
      ast <- runExceptT =<< (Ivy.parse <$> (getSource "Corrupted.ivy"))
      case ast of
        Left _  -> pure ()
        Right v -> expectationFailure $ "Source is parsed: " <> show v
  describe "Parsable.ivy" $ do
    it "should parse" $ do
      ast <- runExceptT =<< (Ivy.parse <$> (getSource "Corrupted.ivy"))
      case ast of
        Left err  -> expectationFailure $ "Source could not be parsed: " <> show err
        Right _ -> pure ()

getSource :: FilePath -> IO T.Text
getSource filename =
  T.pack <$> readFile ("tests" </> "sources" </> filename)
