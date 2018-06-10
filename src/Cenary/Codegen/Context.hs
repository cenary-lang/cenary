module Cenary.Codegen.Context where

import           Cenary.Syntax
import qualified Data.Map as M
import           Prelude hiding (lookup)

newtype Context address = Context (M.Map String (PrimType, address))

newEmptyCtx :: Context address
newEmptyCtx = Context M.empty

add :: String -> (PrimType, address) -> Context address -> Context address
add name addr (Context ctx) = Context $ M.insert name addr ctx

lookup :: String -> Context address -> Maybe (PrimType, address)
lookup name (Context ctx) = M.lookup name ctx
