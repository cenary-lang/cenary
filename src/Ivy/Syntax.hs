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

data Type =
    IntT
  | CharT
  deriving (Eq, Show)

data Expr =
    PrimInt Integer
  | PrimChar Char
  | Identifier Name
  | VarDecl Type Name
  | Assignment Name Expr
  | BinaryOp Op Expr Expr
  | Times Integer Block
  deriving Show
