{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivy.EvmAPI.StackTL where

import GHC.TypeLits
import Control.Monad.Indexed
import Prelude hiding ((>>))
import Data.Kind (Constraint)
import Control.Monad.Indexed.State (IxStateT (..))
import Control.Monad.Indexed.Trans (ilift)

data Stack a = Stack [a]

type family Empty :: Stack k where
  Empty = 'Stack '[]

type family Push (stack :: Stack k) (a :: k) :: Stack k where
  Push ('Stack xs) val = 'Stack (val ': xs)

type family Pop (stack :: Stack k) :: (k, Stack k) where
  Pop ('Stack '[]) = 
    TypeError
      ( 'Text "You cannot pop from an empty stack!"
   ':$$: 'Text "While trying to pop from the stack."
      )

  Pop ('Stack (x ': xs)) = '(x, 'Stack xs)

data Proxy k = Proxy

(>>>>) :: Monad m => IxStateT m i o a -> IxStateT m o j b -> IxStateT m i j b
a >>>> b = a >>>= \_ -> b
infixl 3 >>>>

data StackElem = JumpdestElem
               | IntElem

data MyState (stack :: Stack StackElem) = MyState
  { _bytecode :: String
  , _pc :: Int
  }

data Wrap (elem :: StackElem) where
  JUMPDEST :: Wrap 'JumpdestElem
  INT :: Wrap 'IntElem

ixconst :: Monad m => IxStateT m (MyState stack0) (MyState stack1) ()
ixconst = IxStateT $ \(MyState bc pc) -> pure ((), MyState bc pc)

ixpush :: Monad m => forall x. Wrap x -> IxStateT m (MyState stack) (MyState (stack `Push` x)) ()
ixpush _ = ixconst

ixpop :: (Monad m, '(elem, newStack) ~ Pop stack) => IxStateT m (MyState stack) (MyState newStack) ()
ixpop = ixconst

type family IsIntElem (desc :: Symbol) (x :: StackElem) :: Constraint where
  IsIntElem _ 'IntElem = ()
  IsIntElem desc other = TypeError ('Text "Type of the " ':<>: 'Text desc ':<>: 'Text " should be 'IntElem, but it's " ':<>: 'ShowType other)

type family PopInt (stack :: Stack StackElem) :: Stack StackElem where
  PopInt ('Stack '[]) =
    TypeError
      ( 'Text "You cannot pop from an empty stack!"
   ':$$: 'Text "While trying to pop from the stack."
      )
  PopInt ('Stack ('IntElem ': xs)) = 'Stack xs
  PopInt ('Stack (x ': xs)) = TypeError ('Text "Stack element's type should be integer, but it's " ':<>: 'ShowType x ':<>: 'Text " instead.")

type family PopMany (n :: Nat) (stack :: Stack k) :: Stack k where
  PopMany 0 stack = stack
  PopMany n ('Stack (x ': xs)) = PopMany (n - 1) ('Stack xs)

type family PushMany (val :: k) (n :: Nat) (stack :: Stack k) :: Stack k where
  PushMany _ 0 stack = stack
  PushMany val n ('Stack xs) = PushMany val (n - 1) ('Stack (val ': xs))

type StackModify pop push =
  forall stack m result popped.
    ( Monad m
    , popped ~ PopMany pop stack
    , result ~ PushMany 'IntElem push popped
    )
    => IxStateT m (MyState stack) (MyState result) ()

ixadd
  :: ( Monad m
     , auxStack ~ PopInt stack
     , poppedStack ~ PopInt auxStack
     , resultStack ~ Push poppedStack 'IntElem
     )
  => IxStateT m (MyState stack) (MyState resultStack) ()
ixadd = ixconst

addmy :: StackModify 2 1
addmy = ixconst

type EmptyStack = 'Stack '[]

comp :: IxStateT IO (MyState EmptyStack) (MyState EmptyStack) ()
comp = do
  ixpush INT
  ixpush INT
  ixpush INT
  ilift $ print "wow"
  addmy
  ixpop
  ixpop
  where (>>) = (>>>>)

emptyState :: MyState a
emptyState = MyState "" 0

main :: IO ()
main = do
  (out, _) <- (runIxStateT comp) emptyState
  print out
