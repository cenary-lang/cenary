{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Utils.EvmAsm where

--------------------------------------------------------------------------------
import           Control.Applicative hiding (many)
import           Data.Monoid
import qualified Data.Text            as T
import           Data.String          (IsString)
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Number
import           Text.Parsec.String
import qualified Text.Parsec.Token    as Tok
import           Text.Parsec.Language
import           Text.Printf
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef

integer = Tok.integer lexer

newtype ByteCode = ByteCode { unByteCode :: T.Text }
  deriving (Monoid)

asm :: [T.Text] -> T.Text
asm ix = unByteCode $ asm' ix

asm' :: [T.Text] -> ByteCode
asm' [] = ByteCode ""
asm' (input:xs) =
  case parse instrParser "<instruction_set>" (T.unpack input) of
    Left err -> error (show err)
    Right code' -> code' <> asm' xs

toByteCode :: String -> Integer
toByteCode "ADD" = 0x01

toByteCode1 :: String -> Integer -> [Integer]
toByteCode1 "PUSH" val = [0x60, val]

instrParser :: Parser ByteCode
instrParser = do
  instr <- many upper
  arg <- optionMaybe (space >> hexadecimal)
  case arg of
    Just val -> return $ mconcat $ map (ByteCode . T.pack . printf "%x") $ toByteCode1 instr val
    Nothing -> return $ ByteCode $ T.pack $ printf "%02x" $ toByteCode instr
