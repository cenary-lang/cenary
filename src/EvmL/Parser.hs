{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module EvmL.Parser where

--------------------------------------------------------------------------------
import           Control.Arrow      ((>>>))
import           Data.Functor
import qualified Data.Text          as T
import           Text.Parsec        as P
import           Text.Parsec.Expr
import           Text.Parsec.String (Parser)
import           Data.Functor.Identity
--------------------------------------------------------------------------------
import qualified EvmL.Lexer         as L
--------------------------------------------------------------------------------

type Name = String

data OpType =
    OpMul
  | OpDiv
  | OpSum
  | OpMinus
  | OpLt
  | OpGt
  deriving Show

data Expr =
    If Expr Expr
  | PrimInt Integer
  | BinaryOp OpType Expr Expr
  deriving Show

text :: Stream s m Char => T.Text -> ParsecT s u m T.Text
text input = T.pack <$> string (T.unpack input)

if' :: Parser Expr
if' = do
  text "if"
  many space
  pred' <- expr
  many space
  text "then"
  many space
  body <- expr
  return (If pred' body)
  <?> "if expression"

prims :: Parser Expr
prims = (PrimInt <$> L.integer)
    <?> "primitive"

term :: Parser Integer
term = L.natural

binary s opType = Infix (L.reservedOp s >> return (BinaryOp opType))

binops = [ [ binary "*" OpMul AssocLeft
           , binary "/" OpDiv AssocLeft
           ]
         , [ binary "+" OpSum AssocLeft
           , binary "-" OpMinus AssocLeft
           ]
         , [ binary "<" OpLt AssocLeft
           , binary ">" OpGt AssocLeft
           ]
         ]

expr :: Parser Expr
expr = buildExpressionParser binops factor

factor :: Parser Expr
factor = try (L.parens expr)
   <|> try if'
   <|> prims

parse :: T.Text -> Either ParseError Expr
parse = T.unpack >>> P.parse expr "<unknown>"
