{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module Ivy.Syntax where

type Name = String

newtype Block = Block [Stmt]
  deriving (Show, Eq)

data Op =
    OpAdd
  | OpMul
  | OpSub
  | OpDiv
  | OpMod
  | OpGt
  | OpLt
  | OpEq
  deriving (Eq, Show)

type Length = Integer
type Index = Integer

data PrimType =
    TInt
  | TChar 
  | TBool 
  | TArray Length PrimType
  | TFun PrimType
  deriving Eq

instance Show PrimType where
  show TInt = "integer"
  show TChar = "char"
  show TBool = "boolean"
  show (TArray _ ty) = show ty ++ " array"
  show (TFun _ty) = "function"

data Stmt =
    SVarDecl PrimType Name
  | SDeclAndAssignment PrimType Name Expr
  | SAssignment Name Expr
  | SArrAssignment Name Expr Expr
  | STimes Integer Block
  | SWhile Expr Block
  | SIf Expr Block
  | SIfThenElse Expr Block Block
  | SReturn Expr
  | SExpr Expr
  deriving (Show, Eq)

data FunStmt = FunStmt String [(PrimType, Name)] Block PrimType
  deriving (Show, Eq)

data Expr =
    EInt Integer
  | EChar Char
  | EBool Bool
  | EIdentifier Name
  | EArrIdentifier Name Expr
  | EBinop Op Expr Expr
  | EFunCall String [Expr]
  | EArray Length [Expr]
  deriving (Show, Eq)
