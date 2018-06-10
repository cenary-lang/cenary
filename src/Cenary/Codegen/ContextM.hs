{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Cenary.Codegen.ContextM where

import           Cenary.Codegen.VariableStatus
import           Cenary.Codegen.CodegenError
import qualified Cenary.Codegen.Context as Ctx
import           Cenary.Codegen.Scope
import qualified Cenary.Codegen.MappingOrder as MO
import           Cenary.Codegen.Memory
import           Cenary.Codegen.Evm
import           Cenary.Codegen.Env
import           Cenary.Codegen.CodegenState
import           Cenary.Codegen.Address
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Semigroup ((<>))
import           Control.Lens hiding (contexts)
import Prelude hiding (lookup)
import Cenary.Syntax

-- | Class of monads that are able to read and update context
class ContextM m where
  updateCtx :: (Ctx.Context Address -> (Bool, Ctx.Context Address)) -> m ()
  lookup :: String -> m (VariableStatus VarVariableStatus)
  lookupFun :: String -> m (VariableStatus FuncVariableStatus)
  createCtx :: m ()
  popCtx :: m ()
  updateSig :: Sig -> m ()
  ctxDeclareVar :: Name -> PrimType -> Scope -> m ()
  ctxDefineVar :: Name -> Integer -> m ()
  ctxDefineFunc :: Name -> PrimType -> Integer -> m ()

instance ContextM Evm where
  updateCtx f' =
    env %= (contexts %~ updateCtx' f')
      where
        updateCtx' _ []       = []
        updateCtx' f (ctx:xs) = case f ctx of
          (True, newCtx)  -> newCtx : xs
          (False, oldCtx) -> oldCtx : updateCtx' f xs
  lookup name =
    go =<< _contexts <$> use env
    where
      go :: [Ctx.Context Address] -> Evm (VariableStatus VarVariableStatus)
      go [] = return NotDeclared
      go (ctx:xs) =
        case Ctx.lookup name ctx of
          Just (varTy, VarAddr varAddr scope) -> decideVar (varTy, varAddr) scope
          Just _ -> throwError $ InternalError $ "Another case for context (1)"
          Nothing -> go xs
        where
          decideVar (ty, Nothing)   p = return (Decl ty p)
          decideVar (ty, Just addr) p = return (Def ty addr p)
  lookupFun name =
    go =<< _contexts <$> use env
      where
        go [] = return NotDeclared
        go (ctx:xs) =
          case Ctx.lookup name ctx of
            Just (TFun retTy, FunAddr funAddr) -> return (FunDef retTy funAddr)
            Just _ -> throwError $ InternalError $ "Another case for context (2)"
            Nothing -> go xs
  createCtx = env %= (contexts %~ (Ctx.newEmptyCtx :))
  popCtx = env %= (contexts %~ tail)
  updateSig sig' = env %= (sig .~ sig')
  ctxDeclareVar name ty scope = do
    updateCtx ((True,) . Ctx.add name (ty, VarAddr Nothing scope))
    case ty of
      TMap _ _ -> do
        addr <- alloc scope
        create_mapping_order
        ctxDefineVar name addr
      _ -> pure ()
    where
      create_mapping_order
        :: (MonadState CodegenState m, MonadError CodegenError m)
        => m ()
      create_mapping_order = do
        currentMappingOrder <- use mappingOrder
        case MO.lookup name currentMappingOrder of
          Nothing ->
            mappingOrder %= MO.addMapping name
          Just _ ->
            throwError $ InternalError $ "Multiple mapping order insert attempts for mapping named " <> name
  ctxDefineVar name addr =
    lookup name >>= \case
      NotDeclared ->
        throwError (VariableNotDeclared name (TextDetails "ctxDefineVar"))
      Decl tyL scope -> do
        updateCtx $ \ctx ->
          case Ctx.lookup name ctx of
            Nothing ->
              (False, ctx)
            Just _ ->
              (True, Ctx.add name (tyL, VarAddr (Just addr) scope) ctx)
      Def _ _ _ -> throwError $ InternalError $ "TODO (1)"
  ctxDefineFunc name retTy funPc =
    updateCtx ((True,) . Ctx.add name (TFun retTy, FunAddr funPc))
