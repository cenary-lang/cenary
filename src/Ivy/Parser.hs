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
expr = buildExpressionParser binops factor

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

varDecl :: Parser Expr
varDecl = do
  type' <- try array <|> typeAnnot
  whitespace
  name <- identifier
  return $ EVarDecl type' name
  <?> "variable declaration"

block :: Parser Block
block = Block <$> topLevel

curlied :: Parser a -> Parser a
curlied p = reserved "{" *> p <* reserved "}"

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
  type' <- try array <|> typeAnnot
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

eIdentifier :: Parser Expr
eIdentifier =
  EIdentifier
    <$> identifier
    <?> "identifier"

eIfThenElse :: Parser Expr
eIfThenElse = do
  reserved "if"
  pred <- expr
  tBody <- curlied block
  reserved "else"
  eBody <- curlied block
  return (EIfThenElse pred tBody eBody)

eIf :: Parser Expr
eIf = do
  reserved "if"
  pred <- expr
  body <- curlied block
  return (EIf pred body)

eFunDef :: Parser Expr
eFunDef = do
  retType <-  typeAnnot
  name <- identifier
  reserved "()" -- TODO: Fill with args
  body <- curlied block
  return (EFunDef name body retType)

eFunCall :: Parser Expr
eFunCall = do
  name <- identifier
  reserved "()"
  return (EFunCall name)

factor :: Parser Expr
factor = try (parens expr <?> "parens")
     <|> try timesIterationBegin
     <|> try declAndAssignment
     <|> try prims
     <|> try varDecl
     <|> try arrAssignment
     <|> try assignment
     <|> try eIdentifier
     <|> try eIfThenElse
     <|> try eIf
     <|> try eFunDef
     <|> try eFunCall
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
