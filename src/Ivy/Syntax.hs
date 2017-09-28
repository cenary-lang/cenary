module Ivy.Syntax where

type Name = String

newtype Block = Block [Expr]
  deriving Show

data OpType =
    OpAdd
  | OpMul
  | OpSub
  | OpDiv
  deriving Show

data Type =
  IntT
  deriving (Eq, Show)

data Expr =
    PrimInt Integer
  | Identifier Name
  | VarDecl Type Name
  | Assignment Name Expr
  | BinaryOp OpType Expr Expr
  | Times Integer Block
  deriving Show
