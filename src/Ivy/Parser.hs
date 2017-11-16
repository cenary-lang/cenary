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

primInt :: Parser Expr
primInt =
  EInt <$> integer

primChar :: Parser Expr
primChar =
  EChar <$> charLiteral

primBool :: Parser Expr
primBool =
  fmap EBool $
    try (reserved "true" $> True)
    <|> (reserved "false" $> False)

prims :: Parser Expr
prims = try primInt
    <|> try primChar
    <|> primBool
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
expr = buildExpressionParser binops expr'
  where
    expr' :: Parser Expr
    expr' = try (parens expr <?> "parens")
         <|> try prims
         <|> try eIdentifier
         <?>  "factor"

stmt :: Parser Stmt
stmt = try declAndAssignment
   <|> try times
   <|> try varDecl
   <|> try arrAssignment
   <|> try assignment
   <|> try eIfThenElse
   <|> try eIf

typeAnnot :: Parser PrimType
typeAnnot =
  try (reserved "int" $> TInt)
  <|> try (reserved "char" $> TChar)
  <|> reserved "bool" $> TBool
  <?> "type annotation"

array :: Parser PrimType
array = do
  type' <- typeAnnot
  char '['
  size <- integer
  char ']'
  return (TArray size type')

varDecl :: Parser Stmt
varDecl = do
  type' <- try array <|> typeAnnot
  whitespace
  name <- identifier
  return $ SVarDecl type' name
  <?> "variable declaration"

block :: Parser Block
block = Block <$> topLevel

curlied :: Parser a -> Parser a
curlied p = reserved "{" *> p <* reserved "}"

times :: Parser Stmt
times = do
  until <- integer
  symbol "."
  reserved "times"
  reserved "do"
  body <- block
  reserved "endtimes"
  return (STimes until body)
  <?> "times iteration"

assignment :: Parser Stmt
assignment = do
  name <- identifier
  reserved "="
  rhs <- expr
  return (SAssignment name rhs)
  <?> "assignment"

arrAssignment :: Parser Stmt
arrAssignment = do
  name <- identifier
  char '['
  index <- expr
  char ']'
  whitespace
  reserved "="
  whitespace
  val <- expr
  return (SArrAssignment name index val)
  <?> "array assignment"

declAndAssignment :: Parser Stmt
declAndAssignment = do
  type' <- try array <|> typeAnnot
  whitespace
  name <- identifier
  reserved "="
  rhs <- expr
  return (SDeclAndAssignment type' name rhs)

eIdentifier :: Parser Expr
eIdentifier =
  EIdentifier
    <$> identifier
    <?> "identifier"

eIfThenElse :: Parser Stmt
eIfThenElse = do
  reserved "if"
  pred <- expr
  tBody <- curlied block
  reserved "else"
  eBody <- curlied block
  return (SIfThenElse pred tBody eBody)

eIf :: Parser Stmt
eIf = do
  reserved "if"
  pred <- expr
  body <- curlied block
  return (SIf pred body)

topLevel :: Parser [Stmt]
topLevel =
  many (stmt <* reserved ";")
  <?> "topLevel"

parse :: T.Text -> Either ParseError Stmt
parse = T.unpack >>> P.parse stmt "<unknown>"

parseTopLevel :: T.Text -> Either ParseError [Stmt]
parseTopLevel = T.unpack >>> P.parse topLevel "<unknown>"
