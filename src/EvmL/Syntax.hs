module EvmL.Syntax where

type Name = String

data OpType =
    OpAdd
  | OpMul
  | OpSub
  | OpDiv
  deriving Show

data Expr =
    If Expr Expr
  | PrimInt Integer
  | BinaryOp OpType Expr Expr
  deriving Show
