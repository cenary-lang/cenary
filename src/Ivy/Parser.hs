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
import           Text.Parsec.Char      as PC
import           Text.Parsec.Expr
import           Text.Parsec.String    (Parser)
--------------------------------------------------------------------------------
import           Ivy.Lexer
import           Ivy.Syntax
--------------------------------------------------------------------------------

text :: Stream s m Char => T.Text -> ParsecT s u m T.Text
text input = T.pack <$> string (T.unpack input)

primChar :: Parser Expr
primChar = do
  c <- charLiteral
  return (EChar c)

prims :: Parser Expr
prims = try (EInt <$> integer)
    <|> primChar
    <?> "primitive"

term :: Parser Integer
term = natural

binary s opType = Infix (reservedOp s >> return (EBinop opType))

binops = [ [ binary "*" OpMul AssocLeft
           , binary "/" OpDiv AssocLeft
           ]
         , [ binary "+" OpAdd AssocLeft
           , binary "-" OpSub AssocLeft
           ]
         ]

expr :: Parser Expr
expr = buildExpressionParser binops factor

array :: Parser PrimType
array = do
  type' <- try (string "int" $> TInt) <|> string "char" $> TChar
  char '['
  size <- integer
  char ']'
  return (TArray size type')

primType :: Parser PrimType
primType = try (string "int" $> TInt)
       <|> string "char" $> TChar
       <?> "type declaration"

varDecl :: Parser Expr
varDecl = do
  type' <- try array <|> primType
  whitespace
  name <- identifier
  return $ EVarDecl type' name
  <?> "variable decleration"

block :: Parser Block
block = Block <$> topLevel

timesIterationBegin :: Parser Expr
timesIterationBegin = do
  until <- integer
  symbol "."
  reserved "times"
  reserved "do"
  body <- block
  reserved "endtimes"
  return (ETimes until body)
  <?> "times iteration"

assignment :: Parser Expr
assignment = do
  name <- identifier
  reserved "="
  rhs <- expr
  return (EAssignment name rhs)
  <?> "assignment"

arrAssignment :: Parser Expr
arrAssignment = do
  name <- identifier
  char '['
  index <- expr
  char ']'
  whitespace
  reserved "="
  whitespace
  val <- expr
  return (EArrAssignment name index val)
  <?> "array assignment"

declAndAssignment :: Parser Expr
declAndAssignment = do
  type' <- try array <|> primType
  whitespace
  name <- identifier
  reserved "="
  rhs <- expr
  return (EDeclAndAssignment type' name rhs)

debug :: Parser Expr
debug = do
  reserved "debug"
  char '('
  val <- expr
  char ')'
  return (EDebug val)
  <?> "debug"

factor :: Parser Expr
factor = try (parens expr <?> "parens")
     <|> try timesIterationBegin
     <|> try declAndAssignment
     <|> try prims
     <|> try varDecl
     <|> try arrAssignment
     <|> try assignment
     <|> try (EIdentifier <$> identifier <?> "identifier")
     <|> debug
     <?>  "factor"

topLevel :: Parser [Expr]
topLevel = do
  many (expr <* reserved ";")
  <?> "topLevel"

parse :: T.Text -> Either ParseError Expr
parse = T.unpack >>> P.parse expr "<unknown>"

parseTopLevel :: T.Text -> Either ParseError [Expr]
parseTopLevel = T.unpack >>> P.parse topLevel "<unknown>"
