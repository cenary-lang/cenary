{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ivy.AbiBridge where

import           Control.Monad.Except
import           Data.Semigroup ((<>))
import qualified Data.Text.Lazy as T
import qualified Evm.Abi as Abi
import           Ivy.Codegen.Types (CodegenError (..))
import           Ivy.Error
import qualified Ivy.Syntax as S

toAbiTy :: S.PrimType -> Either String Abi.AbiType
toAbiTy = \case
  S.TInt -> pure Abi.AbiTy_uint256
  S.TChar -> pure Abi.AbiTy_char
  S.TBool -> pure Abi.AbiTy_bool
  ty -> throwError $ "Don't know how to convert type " <> show ty <> " to abi representation"

astToAbi :: MonadError Error m => S.AST -> m T.Text
astToAbi = transformAstToAbi astToAbiIR Abi.encodeAbi

transformAstToAbi
  :: Functor f
  => (ast -> f abiInternal)
  -> (abiInternal -> resultTy)
  -> (ast -> f resultTy)
transformAstToAbi abiInternal abiEncode ast =
  abiEncode <$> abiInternal ast

astToAbiIR :: forall m. MonadError Error m => S.AST -> m Abi.Abi
astToAbiIR = fmap Abi.Abi . mapM func_to_abi
  where
    func_to_abi :: S.FunStmt -> m Abi.Function
    func_to_abi (S.FunStmt (S.FunSig _ name args) _ retTy) = do
      inputs <- mapM arg_to_abi_input args
      case toAbiTy retTy of
        Left err -> throwError $ Codegen $ InternalError err
        Right abi_ret_ty ->
          pure $ Abi.Function
            { Abi._functionName = name
            , Abi._functionType = Abi.FunctionTypeFunction
            , Abi._functionConstant = True
            , Abi._functionPayable = False
            , Abi._functionInputs = inputs
            , Abi._functionOutputs = [ Abi.Output "output_name" abi_ret_ty ]
            }

    arg_to_abi_input :: (S.PrimType, S.Name) -> m Abi.Input
    arg_to_abi_input (ty, name) = Abi.Input name <$> (to_abi_ty ty)

    to_abi_ty :: S.PrimType -> m Abi.AbiType
    to_abi_ty = \case
      S.TInt -> pure Abi.AbiTy_uint256
      S.TChar -> pure Abi.AbiTy_char
      S.TBool -> pure Abi.AbiTy_bool
      ty -> throwError $ Codegen $ InternalError $ "Don't know how to convert type " <> show ty <> " to abi representation"

