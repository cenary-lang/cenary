module Cenary.Codegen.MappingOrder where

import qualified Data.Map as M

newtype MappingOrder = MappingOrder (M.Map String Integer)

empty :: MappingOrder
empty = MappingOrder M.empty

lookup :: String -> MappingOrder -> Maybe Integer
lookup name (MappingOrder mo) = M.lookup name mo

addMapping :: String -> MappingOrder -> MappingOrder
addMapping name (MappingOrder mo) = MappingOrder $ M.insert name (fromIntegral $ M.size mo) mo
