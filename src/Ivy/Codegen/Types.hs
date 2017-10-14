{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ivy.Codegen.Types where

--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger           hiding (logInfo)
import           Control.Monad.Logger.CallStack (logInfo)
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map                       as M
import           Data.Semigroup                 ((<>))
import qualified Data.Text                      as T
--------------------------------------------------------------------------------
import           Ivy.Syntax                     (PrimType(..))
--------------------------------------------------------------------------------

data CodegenError =
    VariableNotDeclared String
  | VariableAlreadyDeclared String
  | VariableNotDefined String
  | TypeMismatch String PrimType PrimType
  | ScopedTypeViolation String PrimType PrimType
  | InternalError String
  | WrongOperandTypes PrimType PrimType

type Addr = Integer
data Operand = Operand PrimType Addr

instance Show CodegenError where
  show (VariableNotDeclared var) = "Variable " <> var <> " is not declared."
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

data Size =
    Size_1
  | Size_2
  | Size_4
  | Size_8
  | Size_32
  deriving (Show, Eq, Ord)

sizeInt :: Size -> Integer
sizeInt Size_1 = 1
sizeInt Size_2 = 2
sizeInt Size_4 = 4
sizeInt Size_8 = 8
sizeInt Size_32 = 32

type Address = Integer
type SymbolTable = M.Map String (PrimType, Maybe Address)

data MemBlock = MemBlock
  { _memBlockIndex     :: Integer
  , _memBlockAllocated :: Integer
  } deriving Show

makeLenses ''MemBlock

type MemPointers = M.Map Size MemBlock

data CodegenState = CodegenState
  { _byteCode    :: !T.Text
  , _memPointers :: !MemPointers
  , _globalScope :: !SymbolTable
  , _localScope  :: !SymbolTable
  , _memory      :: !(M.Map Integer Integer)
  }

makeLenses ''CodegenState

initMemBlock :: MemBlock
initMemBlock = MemBlock 0 0

initMemPointers :: MemPointers
initMemPointers = M.fromList
  [ (Size_1, initMemBlock)
  , (Size_2, initMemBlock)
  , (Size_4, initMemBlock)
  , (Size_8, initMemBlock)
  , (Size_32, initMemBlock)
  ]

newtype Evm a = Evm { runEvm :: StateT CodegenState (LoggingT (ExceptT CodegenError IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CodegenState, MonadError CodegenError, MonadLogger)

type ScopeLevel = Int

data Scope = Local | Global
data VariableStatus = NotDeclared
                    | Decl PrimType Scope
                    | Def PrimType Scope Integer
                    | Error CodegenError

totalMemBlockSize :: Integer
totalMemBlockSize = 32 -- There are 32 bytes in a block

-- O(n)
findMemspace
  :: Size  -- Required size of allocation
  -> Evm (Integer, Integer) -- (newIndex, allocLoc)
findMemspace size = do
  mem <- use memory
  let msize = fromIntegral $ M.size mem
  case go (M.assocs mem) of
    Nothing     -> (memory %= M.insert msize (0 :: Integer)) >> return (msize, 0 :: Integer)
    Just result -> return result
    where
      go :: [(Integer, Integer)] -> Maybe (Integer, Integer)
      go [] = Nothing
      go ((index, alloc):xs) =
        if totalMemBlockSize - alloc > sizeInt size
           then Just (index, alloc + sizeInt size)
           else go xs

calcAddr :: Integer -> Integer -> Integer
calcAddr index allocLen = index * totalMemBlockSize + allocLen

-- O(logn)
updateMemPointer
  :: Size   -- Location where allocation shall start inside block
  -> Integer    -- Index of the block
  -> Integer    -- New allocated size
  -> Evm ()
updateMemPointer size index newAllocSize = do
  memptrs <- use memPointers
  logInfo $ T.pack $ show $ memptrs
  memPointers %= M.alter alter' size
  where
    alter' :: Maybe MemBlock -> Maybe MemBlock
    alter' Nothing =
      error $ "Pointer does not exist for size: " <> show size
    alter' (Just (MemBlock old_index old_alloc)) =
      Just (MemBlock index newAllocSize)

alloc :: Size -> Evm Integer
alloc size = do
  memPtrs <- use memPointers
  case M.lookup size memPtrs of
    Nothing -> throwError $ InternalError $ "Pointer does not exist: " <> show size
    Just (MemBlock index alloc) ->
      if totalMemBlockSize - alloc >= sizeInt size
        then
        let
          newPtr :: Integer = (alloc + sizeInt size)
        in do
          updateMemPointer size index newPtr
          return (calcAddr index alloc)
      else do
          (newIndex, allocLoc) <- findMemspace size
          let newPtr = allocLoc + sizeInt size
          updateMemPointer size newIndex newPtr
          return (calcAddr newIndex newPtr)

allocBulk
  :: Integer
  -> Size
  -> Evm Integer
allocBulk length size = do
  mem <- use memory
  let msize = fromIntegral $ M.size mem
  if sizeInt size * length <= totalMemBlockSize
     then -- There are 5 blocks of 4 bytes
       memory %= M.update (updateInc size length) msize
     else do -- There are 15 blocks of 4 bytes
       let fitinLength = totalMemBlockSize `div` sizeInt size -- 32 / 4 = 8 mem blocks can fit in
       memory %= M.update (updateInc size fitinLength) msize
       void $ allocBulk (length - fitinLength) size
  return $ calcAddr msize (0 :: Integer)
    where
      updateInc :: Size -> Integer -> Integer -> Maybe Integer
      updateInc _ 0 allocated = Just allocated
      updateInc size length allocated = updateInc size (length - 1) (allocated + sizeInt size)

sizeof :: PrimType -> Size
sizeof IntT = Size_8
sizeof CharT = Size_1
sizeof other = error $ "`sizeof` is not implemented for type " <> show other

initCodegenState :: CodegenState
initCodegenState = CodegenState
  { _byteCode   = ""
  , _memPointers = initMemPointers
  , _globalScope = M.empty
  , _localScope = M.empty
  , _memory     = M.empty
  }
