module Ivy.Syntax where

type Name = String

newtype Block = Block [Expr]
  deriving Show

data Op =
    OpAdd
  | OpMul
  | OpSub
  | OpDiv
  deriving Show

type Length = Integer
type Index = Integer

data PrimType =
    TInt
  | TChar
  | TArray Length PrimType
  deriving (Eq, Show)

data Expr =
    EInt Integer
  | EChar Char
  | EIdentifier Name
  | EVarDecl PrimType Name
  | EAssignment Name Expr
  | EDeclAndAssignment PrimType Name Expr
  | EArrAssignment Name Index Expr
  | EBinop Op Expr Expr
  | ETimes Integer Block
  | EDebug Expr
  deriving Show
