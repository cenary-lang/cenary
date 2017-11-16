module Ivy.Syntax where

type Name = String

newtype Block = Block [Stmt]
  deriving (Eq, Show)

data Op =
    OpAdd
  | OpMul
  | OpSub
  | OpDiv
  deriving (Eq, Show)

type Length = Integer

data PrimType =
    TInt
  | TChar
  | TBool
  | TArray Length PrimType
  deriving (Eq, Show)

data Stmt =
    SVarDecl PrimType Name
  | SDeclAndAssignment PrimType Name Expr
  | SAssignment Name Expr
  | SArrAssignment Name Expr Expr
  | STimes Integer Block
  | SIf Expr Block
  | SIfThenElse Expr Block Block
  deriving (Eq, Show)

data Expr =
    EInt Integer
  | EChar Char
  | EBool Bool
  | EIdentifier Name
  | EBinop Op Expr Expr
  deriving (Eq, Show)
