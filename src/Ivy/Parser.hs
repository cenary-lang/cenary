{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ivy.Parser where

--------------------------------------------------------------------------------
import           Control.Arrow ((>>>))
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

litInt :: Parser Expr
litInt =
  EInt <$> integer

litChar :: Parser Expr
litChar =
  EChar <$> charLiteral

litBool :: Parser Expr
litBool =
  fmap EBool $
    try (reserved "true" $> True)
    <|> (reserved "false" $> False)

litString :: Parser Expr
litString = do
  str <- map EChar <$> stringLiteral
  return (EArr (toInteger (length str)) str)

litArray :: Parser Expr
litArray = do
  elems <- curlied (commaSep eLit)
  return (EArr (toInteger (length elems)) elems)

litDynArray :: Parser Expr
litDynArray = do
  EDynArr <$ curlied (pure ())

eLit :: Parser Expr
eLit = try litInt
    <|> try litChar
    <|> try litBool
    <|> try litString
    <|> try litArray
    <|> litDynArray
    <?> "primitive"

term :: Parser Integer
term = natural

binary :: String -> Op -> Assoc -> Operator String () Identity Expr
binary s opType = Infix (reservedOp s >> return (EBinop opType))

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
         <|> try eLit
         <|> try eFunCall
         <|> try eArrIdentifier
         <|> eIdentifier
         <?>  "factor"

stmt :: Parser Stmt
stmt =
  try declAndAssignment
   <|> try while
   <|> try varDecl
   <|> try arrAssignment
   <|> try assignment
   <|> try sIfThenElse
   <|> try sReturn
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
  size <- integer
  char' ']'
  return (TArray size type')

typedIdentifier :: Parser (PrimType, Name)
typedIdentifier = do
  type' <- try tyArray <|> typeAnnot
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

curlied :: Parser a -> Parser a
curlied p = reserved "{" *> p <* reserved "}"

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
  reserved "="
  whitespace
  val <- expr
  return (SArrAssignment name index val)
  <?> "array assignment"

declAndAssignment :: Parser Stmt
declAndAssignment = do
  type' <- try tyArray <|> typeAnnot
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
  char' '['
  index <- expr
  char' ']'
  return (EArrIdentifier name index)

sIfThenElse :: Parser Stmt
sIfThenElse = do
  reserved "if"
  pred <- expr
  tBody <- curlied block
  reserved "else"
  eBody <- curlied block
  return (SIfThenElse pred tBody eBody)
  <?> "if then else"

funModifier :: Parser FunModifier
funModifier =
  reserved "pure" $> PureModifier

sFunDef :: Parser FunStmt
sFunDef = do
  modifiers <- many funModifier
  retType <- try tyArray <|> typeAnnot
  whitespace
  name <- identifier
  whitespace
  char' '('
  args <- commaSep typedIdentifier
  char' ')'
  whitespace
  body <- curlied block
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

parse :: T.Text -> Either ParseError [FunStmt]
parse = T.unpack >>> P.parse (P.many sFunDef) "<stmt-toplevel>"
