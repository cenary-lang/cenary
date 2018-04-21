module Ivy.Types where

instance Loadable (Operand 'StackVariable) where
  loadPoly _ = pure ()

instance Loadable (Operand 'HeapVariable) where
  loadPoly _ = pure ()

class MemoryM m where
  storeAddressed
    :: Integer -- Address of the value. Value should be loaded from this address
    -> Integer -- Address to put value on
    -> m ()
  load
    :: Integer
    -> m ()
  storeVal
    :: Integer -- Actual value
    -> Integer -- Address to put value on
    -> m ()
  alloc
    :: Size
    -> m Integer
  store
    :: m ()
  push
    :: Integer
    -> m ()
  allocBulk
    :: Integer
    -> Size
    -> m Integer

instance MemoryM Evm where
  storeAddressed valAddr destAddr = do
    -- Initial state
    load valAddr
    push32 destAddr
    store

  load addr = do
    push32 addr
    mload

  storeVal val destAddr = do
    push32 val
    push32 destAddr
    store

  store =
    mstore

  alloc size = do
    pointer <- use memPointer
    mem <- use memory
    let (Just addr) = flip find [pointer..] $ \ix ->
          case M.lookup ix mem of
            Nothing -> True
            Just _  -> False
    memory %= M.insert addr size
    memPointer .= addr
    pure (addr * 0x20)

  push val =
    push32 val -- OPTIMIZE: different PUSH variants can be used for this task

  allocBulk len size = do
    pointer <- use memPointer
    mem <- use memory
    let group_existence :: [[(Integer, Maybe Size)]] =
          groupBy ((==) `on` snd)
          $ map (\ix -> (ix, M.lookup ix mem))
          $ [pointer..]
    let (Just cellGroup) = flip find group_existence $ \cells ->
          case take (fromIntegral len) cells of
            [] -> False
            ((_, Just _):_) -> False -- These are filled cells
            suitable ->
              if fromIntegral (length suitable) >= len
                 then True
                 else False

    let (startAddr, _) = head cellGroup
    forM_ [startAddr..(startAddr + len - 1)] $ \addr -> memory %= M.update (const (Just size)) addr
    pure (startAddr * 0x20)

