{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ivy.Parser where

--------------------------------------------------------------------------------
import           Data.Functor
import           Data.Functor.Identity
import qualified Data.Text as T
import           Prelude hiding (pred, until)
import           Text.Parsec as P
import           Text.Parsec.Expr
import           Text.Parsec.String (Parser)
--------------------------------------------------------------------------------
import           Ivy.Lexer
import           Ivy.Syntax
--------------------------------------------------------------------------------

char' :: Char -> Parser ()
char' = void . char

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

primString :: Parser Expr
primString = do
  str <- map EChar <$> stringLiteral
  return (EArray str)

eArray :: Parser Expr
eArray = do
  elems <- curlied (commaSep ePrim)
  return (EArray elems)

ePrim :: Parser Expr
ePrim = try primInt
    <|> try primChar
    <|> try primBool
    <|> try primString
    <|> eArray
    <?> "primitive"

term :: Parser Integer
term = natural

binary :: String -> Op -> Assoc -> Operator String () Identity Expr
binary s opType = Infix (whitespace >> reservedOp s >> whitespace >> return (EBinop opType))

binops :: [[Operator String () Identity Expr]]
binops = [ [ binary "*" OpMul AssocLeft
           , binary "/" OpDiv AssocLeft
           , binary "%" OpMod AssocLeft
           ]
         , [ binary "+" OpAdd AssocLeft
           , binary "-" OpSub AssocLeft
           ]
         , [ binary ">" OpGt AssocLeft
           , binary "<" OpLt AssocLeft
           , binary "==" OpEq AssocLeft
           ]
         ]

expr :: Parser Expr
expr = buildExpressionParser binops expr'
  where
    expr' :: Parser Expr
    expr' = try (parens expr <?> "parens")
         <|> try ePrim
         <|> try eFunCall
         <|> try eArrIdentifier
         -- <|> try eMapIdentifier
         <|> eIdentifier
         <?>  "factor"

stmt :: Parser Stmt
stmt =
  try declAndAssignment
   <|> try while
   <|> try arrAssignment
   <|> try mapAssignment
   <|> try assignment
   <|> try sIfThenElse
   <|> try sResize
   <|> try sReturn
   <|> try varDecl
   <|> try sExpr
   <|> sIf
   <?> "Statement"

sExpr :: Parser Stmt
sExpr = SExpr <$> expr

sReturn :: Parser Stmt
sReturn = do
  reserved "return"
  retExpr <- expr
  return (SReturn retExpr)

typeAnnot :: Parser PrimType
typeAnnot =
  try (reserved "int" $> TInt)
  <|> try (reserved "char" $> TChar)
  <|> reserved "bool" $> TBool
  <?> "type annotation"

tyArray :: Parser PrimType
tyArray = do
  type' <- typeAnnot
  char' '['
  char' ']'
  return (TArray type')

tyMap :: Parser PrimType
tyMap = do
  reserved "mapping"
  TMap
    <$> around '[' anyType ']'
    <*> around '[' anyType ']'

anyType :: Parser PrimType
anyType = try tyMap
      <|> try tyArray
      <|> typeAnnot

typedIdentifier :: Parser (PrimType, Name)
typedIdentifier = do
  type' <- anyType
  whitespace
  name <- identifier
  return (type', name)

varDecl :: Parser Stmt
varDecl = do
  (type', name) <- typedIdentifier
  return $ SVarDecl type' name
  <?> "variable declaration"

block :: Parser Block
block = Block <$> many (stmt <* reserved ";")

around :: Char -> Parser a -> Char -> Parser a
around l p r = whitespace *> char' l *> whitespace *> p <* whitespace <* char' r <* whitespace

curlied :: Parser a -> Parser a
curlied p = around '{' p '}'

while :: Parser Stmt
while = do
  reserved "while"
  pred <- expr
  body <- curlied block
  return (SWhile pred body)

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
  char' '['
  index <- expr
  char' ']'
  whitespace
  char' '='
  whitespace
  val <- expr
  return (SArrAssignment name index val)
  <?> "array assignment"

mapAssignment :: Parser Stmt
mapAssignment = do
  name <- identifier
  key <- around '{' expr '}'
  whitespace
  char' '='
  val <- expr
  return (SMapAssignment name key val)

declAndAssignment :: Parser Stmt
declAndAssignment = do
  type' <- anyType
  whitespace
  name <- identifier
  reserved "="
  rhs <- expr
  return (SDeclAndAssignment type' name rhs)
  <?> "decl and assignment"

eIdentifier :: Parser Expr
eIdentifier =
  EIdentifier
    <$> identifier
    <?> "identifier"

eArrIdentifier :: Parser Expr
eArrIdentifier = do
  name <- identifier
  index <- around '[' expr ']'
  return (EArrIdentifier name index)

eMapIdentifier :: Parser Expr
eMapIdentifier = do
  name <- identifier
  key <- around '{' expr '}'
  return (EMapIdentifier name key)

sIfThenElse :: Parser Stmt
sIfThenElse = do
  reserved "if"
  pred <- expr
  tBody <- curlied block
  reserved "else"
  eBody <- curlied block
  return (SIfThenElse pred tBody eBody)
  <?> "if then else"

sResize :: Parser Stmt
sResize = do
  reserved "resize"
  around '(' args ')'
  where
    args = do
      id' <- identifier
      whitespace >> char' ',' >> whitespace
      size <- expr
      return (SResize id' size)
  
funModifier :: Parser FunModifier
funModifier =
  reserved "pure" $> PureModifier

sFunDef :: Parser FunStmt
sFunDef = do
  whitespace
  modifiers <- many funModifier
  retType <- anyType
  whitespace
  name <- identifier
  whitespace
  char' '('
  args <- commaSep typedIdentifier
  char' ')'
  whitespace
  body <- curlied block
  whitespace
  return (FunStmt (FunSig modifiers name args) body retType)
  <?> "function definition"

eFunCall :: Parser Expr
eFunCall = do
  name <- identifier
  char' '('
  args <- commaSep expr
  char' ')'
  return (EFunCall name args)
  <?> "function call"

sIf :: Parser Stmt
sIf = do
  reserved "if"
  pred <- expr
  body <- curlied block
  return (SIf pred body)
  <?> "if statement"

parse :: T.Text -> Either ParseError [GlobalStmt]
parse = P.parse p "<stmt-toplevel>" . T.unpack
  where
    func = GlobalFunc <$> try sFunDef
    decl = GlobalDecl <$> stmt
    p = many $ (try func <|> decl)
             <* char' ';'
             <* whitespace
