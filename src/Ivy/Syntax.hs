{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Ivy.Syntax where

import Data.Monoid ((<>))
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

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
  show TInt          = "uint256" -- We just send integers in transactions for now.
  show TChar         = "char"
  show TBool         = "boolean"
  show (TArray _ ty) = show ty ++ " array"
  show (TFun _ty)    = "function"

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

data FunSig = FunSig String [(PrimType, Name)]
  deriving (Eq)

instance Show FunSig where
  show (FunSig name args) =
    let show_args [] = ""
        show_args [(ty, name)] = show ty
        show_args ((ty, name):xs) = show ty <> "," <> show_args xs
     in name <> "(" <> show_args args <> ")"

data FunStmt = FunStmt FunSig Block PrimType
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
