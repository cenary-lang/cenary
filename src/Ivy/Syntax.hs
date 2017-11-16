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

data PrimType =
    TInt
  | TChar
  | TBool
  | TArray Length PrimType
  deriving (Eq, Show)

data Expr =
    EInt Integer
  | EChar Char
  | EBool Bool
  | EIdentifier Name
  | EVarDecl PrimType Name
  | EAssignment Name Expr
  | EDeclAndAssignment PrimType Name Expr
  | EArrAssignment Name Expr Expr
  | EBinop Op Expr Expr
  | ETimes Integer Block
  | EDebug Expr
  | EIf Expr Block
  | EIfThenElse Expr Block Block
  | EFunDef String Block PrimType
  | EFunCall String
  | EReturn Expr
  deriving Show
