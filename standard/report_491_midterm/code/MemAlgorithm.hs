-- code/MemAlgorithm.hs

alloc :: Size -> Evm Integer
alloc size = do
  memPtrs <- use memPointers
  case M.lookup size memPtrs of
    Nothing -> throwError $ InternalError $ "Pointer does not exist: " <> show size
    Just (MemBlock index alloc) ->
      if totalMemBlockSize - alloc >= sizeInt size
        then
        let
          newPos :: Integer = (alloc + sizeInt size)
        in do
          updateMemPointer size index newPos
          let baseAddr = calcAddr index alloc
          let targetAddr = calcAddr index newPos
          markMemAlloc index targetAddr
          return baseAddr
      else do
          newIndex <- findMemspace
          let baseAddr = 0
          let targetAddr = sizeInt size
          updateMemPointer size newIndex targetAddr
          markMemAlloc newIndex targetAddr
          return (calcAddr newIndex baseAddr)

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
