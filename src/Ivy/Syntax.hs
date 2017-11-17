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
  | TFun PrimType -- Return type and its value's address
  deriving (Eq, Show)

data Stmt =
    SVarDecl PrimType Name
  | SDeclAndAssignment PrimType Name Expr
  | SAssignment Name Expr
  | SArrAssignment Name Expr Expr
  | STimes Integer Block
  | SIf Expr Block
  | SIfThenElse Expr Block Block
  | SFunDef String Block PrimType
  | SReturn Expr
  | SExpr Expr
  deriving (Eq, Show)

data Expr =
    EInt Integer
  | EChar Char
  | EBool Bool
  | EIdentifier Name
  | EBinop Op Expr Expr
  | EFunCall String
  deriving (Eq, Show)
