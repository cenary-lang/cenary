module Ivy.Syntax where

type Name = String

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
  deriving Show
