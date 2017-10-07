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

type Size = Integer
type Length = Integer
type Index = Integer

data PrimType =
    IntT
  | CharT
  | Array Length PrimType
  deriving (Eq, Show)

data Expr =
    IntExpr Integer
  | CharExpr Char
  | Identifier Name
  | VarDecl PrimType Name
  | Assignment Name Expr
  | ArrAssignment Name Index Expr
  | BinaryOp Op Expr Expr
  | Times Integer Block
  | Debug Expr
  deriving Show
