{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Ivy.Syntax where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type AST = [FunStmt]

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

data PrimType =
    TInt
  | TChar
  | TBool
  | TArray PrimType
  | TFun PrimType
  deriving (Show, Eq)

data Stmt =
    SVarDecl PrimType Name
  | SDeclAndAssignment PrimType Name Expr
  | SAssignment Name Expr
  | SArrAssignment Name Expr Expr
  | SWhile Expr Block
  | SIf Expr Block
  | SIfThenElse Expr Block Block
  | SReturn Expr
  | SExpr Expr
  deriving (Show, Eq)

data FunModifier =
  PureModifier
  deriving (Show, Eq)

data FunSig = FunSig [FunModifier] String [(PrimType, Name)]
  deriving (Show, Eq)

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
  | EArray [Expr]
  deriving (Show, Eq)
