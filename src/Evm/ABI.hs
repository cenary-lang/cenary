{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Evm.Abi
  ( Abi (..)
  , AbiType (..)
  , Input (..)
  , Output (..)
  , Function (..)
  , encodeAbi
  , FunctionType (..)
  ) where

import           Data.Aeson as JSON
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Semigroup ((<>))

encodeAbi :: Abi -> TL.Text
encodeAbi = TL.decodeUtf8 . encode

data Abi = Abi [Function]

data Function = Function
  { _functionName     :: String
  , _functionType     :: FunctionType
  , _functionConstant :: Bool
  , _functionPayable  :: Bool
  , _functionInputs   :: [Input]
  , _functionOutputs  :: [Output]
  }

-- | Need to dig https://web3js.readthedocs.io/en/1.0/web3-eth-abi.html to find other possible types, but mendokusai
data FunctionType = FunctionTypeFunction

instance ToJSON FunctionType where
  toJSON FunctionTypeFunction = "function"

data Input = Input
  { _inputName :: String
  , _inputType :: AbiType
  }

data Output = Output
  { _outputName :: String
  , _outputType :: AbiType
  }

data AbiType =
    AbiTy_uint256
  | AbiTy_string
  | AbiTy_char
  | AbiTy_bool
  | AbiTy_arrayOf AbiType

instance Show AbiType where
  show AbiTy_uint256 = "uint256"
  show AbiTy_string  = "string"
  show AbiTy_char    = "bytes1"
  show AbiTy_bool    = "bool"
  show (AbiTy_arrayOf ty) = show ty <> "[]"

instance ToJSON AbiType where
  toJSON ty = JSON.String (T.pack (show ty))

deriveToJSON (aesonPrefix camelCase) ''Input
deriveToJSON (aesonPrefix camelCase) ''Output
deriveToJSON (aesonPrefix camelCase) ''Function
deriveToJSON (aesonPrefix camelCase) ''Abi
