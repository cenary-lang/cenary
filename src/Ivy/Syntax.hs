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

data Expr =
    PrimInt Integer
  | Identifier Name
  | VarDecl Name
  | Assignment Name Expr
  | BinaryOp OpType Expr Expr
  | Times Integer Block
  deriving Show
