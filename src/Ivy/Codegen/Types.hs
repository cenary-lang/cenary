{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Ivy.Codegen.Types where

--------------------------------------------------------------------------------
import           Control.Lens hiding (Context)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map as M
import           Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
--------------------------------------------------------------------------------
import           Ivy.Syntax (Expr, PrimType (..), Stmt)
--------------------------------------------------------------------------------

data Instruction =
    STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | MOD
  | GT
  | LT
  | EQ
  | ISZERO
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | JUMP
  | JUMPI
  | JUMPDEST
  | CODECOPY
  | PUSH1 Integer
  | PUSH4 Integer
  | PUSH32 Integer
  | DUP1
  | EXP
  | CALLDATALOAD
  | DUP2
  | SWAP1
  | SWAP2
  | LOG0
  | LOG1
  | LOG2
  | RETURN
  | ADDRESS
  deriving Show

data ErrorDetails = NoDetails
                  | ExprDetails Expr
                  | StmtDetails Stmt
                  | TextDetails String
                  deriving Show -- TODO: we can use a manual instance declaration, actually.

data CodegenError =
    MainFunctionDoesNotExist
  | VariableNotDeclared String ErrorDetails
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
  | IllegalArrAccess String PrimType
  | SupportError String
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
  show (IllegalArrAccess name ty) = "You wanted to access to variable " <> name <> " as if it's an array, but it's type is " <> show ty
  show NoReturnStatement = "Functions should have return statement as their last statement"
  show MainFunctionDoesNotExist = "Main function does not exist"
  show (SupportError description) = "Not supported yet: " <> description

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

type Context = M.Map String (PrimType, Address)

data Sig = Sig String [(PrimType, String)] PrimType

data Env = Env
  { _sig :: Sig
  , _contexts :: [Context] -- Essentially a stack
  }

makeLenses ''Env

data MemBlock = MemBlock
  { _memBlockIndex     :: Integer
  , _memBlockAllocated :: Integer
  } deriving Show

makeLenses ''MemBlock

type MemPointers = M.Map Size MemBlock

data FuncRegistryArgInfo = FuncRegistryArgInfo
  { _argTy :: PrimType
  , _argName :: String
  }

data FuncRegistry = FuncRegistry
  { _argsAddresses :: !(M.Map String [FuncRegistryArgInfo])
  }

makeLenses ''FuncRegistry

-- | TODO: This should have its own module
newtype Program = Program { _unProgram :: (Seq.Seq Instruction) }

makeLenses ''Program

initProgram :: Program
initProgram = Program Seq.empty


addInstr :: Instruction -> Program -> Program
addInstr instr p = p & unProgram %~ (instr <|)

data CodegenState = CodegenState
  { _memPointer   :: !Integer
  , _env          :: !Env
  , _memory       :: !(M.Map Integer Size) -- Every memory cell is 32 bytes. If we allocate any of them, we do it with a size
  , _pc           :: !Integer
  , _funcRegistry :: !FuncRegistry
  , _program      :: Program
  , _funcOffset   :: !Integer
  }

makeLenses ''CodegenState

newtype Evm a = Evm { runEvm :: StateT CodegenState (Either CodegenError) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadError CodegenError, MonadFix)

type ScopeLevel = Int

data VarsVar
data VarsFun

data VariableStatus a where
  NotDeclared :: VariableStatus a
  Decl        :: PrimType -> VariableStatus VarsVar
  Def         :: PrimType -> Integer -> VariableStatus VarsVar
  FunDef      :: PrimType -> Integer -> Integer -> VariableStatus VarsFun
