{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ivy.Parser where

--------------------------------------------------------------------------------
import           Control.Arrow         ((>>>))
import           Data.Functor
import           Data.Functor.Identity
import qualified Data.Text             as T
import           Text.Parsec           as P
import           Text.Parsec.Expr
import           Text.Parsec.String    (Parser)
--------------------------------------------------------------------------------
import qualified Ivy.Lexer            as L
import           Ivy.Syntax
--------------------------------------------------------------------------------

text :: Stream s m Char => T.Text -> ParsecT s u m T.Text
text input = T.pack <$> string (T.unpack input)

prims :: Parser Expr
prims = (PrimInt <$> L.integer)
    <?> "primitive"

term :: Parser Integer
term = L.natural

binary s opType = Infix (L.reservedOp s >> return (BinaryOp opType))

binops = [ [ binary "*" OpMul AssocLeft
           , binary "/" OpDiv AssocLeft
           ]
         , [ binary "+" OpAdd AssocLeft
           , binary "-" OpSub AssocLeft
           ]
         ]

expr :: Parser Expr
expr = buildExpressionParser binops factor

varDecl :: Parser Expr
varDecl = do
  L.reserved "var"
  L.whitespace
  name <- L.identifier
  return $ VarDecl name

assignment :: Parser Expr
assignment = do
  name <- L.identifier
  L.whitespace
  L.reserved "="
  L.whitespace
  val <- expr
  return (Assignment name val)

factor :: Parser Expr
factor = try (L.parens expr)
     <|> try prims
     <|> try assignment
     <|> try varDecl
     <|> (Identifier <$> L.identifier)

topLevel :: Parser [Expr]
topLevel = many $ do
  expr' <- expr
  L.reserved ";"
  return expr'

parse :: T.Text -> Either ParseError Expr
parse = T.unpack >>> P.parse expr "<unknown>"

parseTopLevel :: T.Text -> Either ParseError [Expr]
parseTopLevel = T.unpack >>> P.parse topLevel "<unknown>"
