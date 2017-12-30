{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Ivy.Codegen.Types where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens                   hiding (Context)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor                   (($>))
import           Data.Functor.Identity
import qualified Data.Map                       as M
import           Data.Semigroup                 ((<>))
import qualified Data.Text                      as T
--------------------------------------------------------------------------------
import           Ivy.Syntax                     (Expr, PrimType (..), Stmt)
--------------------------------------------------------------------------------

data ErrorDetails = NoDetails
                  | ExprDetails Expr
                  | StmtDetails Stmt
                  | TextDetails String
                  deriving Show -- TODO: we can use a manual instance declaration, actually.

data CodegenError =
    VariableNotDeclared String ErrorDetails
  | VariableAlreadyDeclared String
  | VariableNotDefined String
  | TypeMismatch String PrimType PrimType
  | ScopedTypeViolation String PrimType PrimType
  | InternalError String
  | WrongOperandTypes PrimType PrimType
  | FuncArgLengthMismatch String Int Int
  | ArrayElementsTypeMismatch PrimType PrimType
  | EmptyArrayValue
  | NonInitializedArrayAccess String
  | NoReturnStatement

type Addr = Integer

data Operand = Operand
  { _operandType :: PrimType
  , _operandAddr :: Addr
  }

makeLenses ''Operand

instance Show CodegenError where
  show (VariableNotDeclared var details) = "Variable " <> var <> " is not declared. Details: " ++ show details
  show (VariableNotDefined var)  = "Variable " <> var <> " is not defined."
  show (VariableAlreadyDeclared var) = "Variable " <> var <> " is already declared."
  show (TypeMismatch name expected actual) = "Type mismatch for variable "
                                          <> name
                                          <> ". Expected: "
                                          <> show expected
                                          <> " , actual: "
                                          <> show actual
  show (ScopedTypeViolation name global local) = "TypeScopeViolation for variable "
                                              <> name
                                              <> ". In global scope, it has "
                                              <> show global
                                              <> " while in local scope it has "
                                              <> show local
  show (InternalError err) = "InternalError: " <> err
  show (WrongOperandTypes tyL tyR) = "Wrong operand types: "
                                   <> "Expected a value of "
                                   <> show tyL
                                   <> " but a value of type "
                                   <> show tyR
                                   <> " is provided."

  show (FuncArgLengthMismatch name expected given) = "Function "
                                                  <> name
                                                  <> " expected "
                                                  <> show expected
                                                  <> " arguments, but you have given "
                                                  <> show given

  show (ArrayElementsTypeMismatch ty1 ty2) = "Array has elements from different types: type " <> show ty1 <> " and type " <> show ty2
  show EmptyArrayValue = "Sorry! We can't handle type polymorphism right now, so you should not create empty arrays as right-hand-side values"
  show (NonInitializedArrayAccess name) = "Array " <> name <> " is not initialized yet, and you wanted to access an element of it"
  show NoReturnStatement = "Functions should have return statement as their last statement"

data Size =
    Size_1
  | Size_2
  | Size_4
  | Size_8
  | Size_32
  deriving (Show, Eq, Ord)

type ParamBaseAddrs = (Integer,     Integer)
                  -- ^ Param count, Base address

data Address = VarAddr (Maybe Integer)
             -- ^      Variable adress
             | FunAddr Integer Integer
             -- ^      PC      Return addr
             deriving Show

data Sig = Sig String [(PrimType, String)] PrimType
type Env = (Sig, [Context]) -- A stack
type Context = M.Map String (PrimType, Address)

data MemBlock = MemBlock
  { _memBlockIndex     :: Integer
  , _memBlockAllocated :: Integer
  } deriving Show

makeLenses ''MemBlock

type MemPointers = M.Map Size MemBlock

type FuncRegistry = M.Map String [(PrimType, String, Integer)]

data CodegenState = CodegenState
  { _byteCode         :: !T.Text
  , _memPointers      :: !MemPointers
  , _env              :: !Env
  , _memory           :: !(M.Map Integer Integer)
  , _pc               :: Integer
  , _funcRegistry     :: FuncRegistry
  }

makeLenses ''CodegenState

newtype Evm a = Evm { runEvm :: StateT CodegenState (Either CodegenError) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadError CodegenError)

type ScopeLevel = Int

data VariableStatus = NotDeclared
                    | Decl PrimType
                    | Def PrimType Integer
                    | FunDef PrimType Integer Integer
                    -- ^ FunDef retTy funAddr retAddr
                    | Error CodegenError
