{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cenary.EvmAPI.StackTL where

import           Control.Monad.Indexed
import           GHC.TypeLits
import           Prelude hiding ((>>))

data StackElem = Jumpdest

data Stack (name :: Symbol) (elems :: [StackElem]) = Stack

type family Push (elem :: *) (s :: *) :: * where
  e `Push` (Stack name xs) = Stack name ((TyToElem e) ': xs)

type family Pop s :: (StackElem, *) where
  Pop (Stack name (x ': xs)) = '(x, Stack name xs)
  Pop (Stack name '[]) =
    TypeError
      ( 'Text "You cannot pop from an empty stack!"
   ':$$: 'Text "While trying to pop from the stack named " ':<>: 'ShowType name
      )

push :: (newStack ~ Push ty givenStack) => givenStack -> ty -> m newStack
push _ _ = undefined

type family ElemToTy (s :: StackElem) :: * where
  ElemToTy 'Jumpdest = Int

type family TyToElem (s :: *) :: StackElem where
  TyToElem Int = 'Jumpdest

pop :: (Monad m, '(elem, newStack) ~ Pop givenStack) => givenStack -> m (ElemToTy elem, newStack)
pop = undefined

type EmptyStack = Stack "MyStack" '[]

newtype IxState is os a = IxState { runIState :: is -> (os, a) }

instance IxFunctor IxState where
  imap f (IxState stateF) = IxState $ \is ->
    let (os, a) = stateF is
     in (os, f a)

instance IxPointed IxState where
  ireturn v = IxState (, v)

instance IxApplicative IxState where
  iap (IxState stateFf) (IxState stateF) = IxState $ \is ->
    let (os, f) = stateFf is
        (os2, a) = stateF os
     in (os2, f a)

instance IxMonad IxState where
  ibind f (IxState ija) = IxState $ \i ->
    let (j, a) = ija i
        IxState jkb = f a
     in jkb j

toBC :: StackElem -> String
toBC Jumpdest = "0x02"

ixpush :: x -> IxState (MyState stack) (MyState (x `Push` stack)) ()
ixpush _ = IxState $ \(MyState bc) -> (MyState bc, ())

ixpop :: ('(elem, newStack) ~ Pop stack) => IxState (MyState stack) (MyState newStack) ()
ixpop = IxState $ \(MyState bc) -> (MyState bc, ())

ixop :: Show x => x -> IxState (MyState stack) (MyState stack) ()
ixop x = IxState $ \(MyState bc) -> (MyState (bc ++ show x), ())

type family Shrink (vals :: [a]) (n :: Nat) :: [a] where
  Shrink xs 0 = xs
  Shrink (x ': xs) v = Shrink xs (v - 1)

type family Extend (val :: a) (vals :: [a]) (n :: Nat) :: [a] where
  Extend _ xs 0 = xs
  Extend x xs n = Extend x (x ': xs) (n - 1)

type StackModify pop push a = forall name x xs. IxState (MyState (Stack name (Extend x xs pop))) (MyState (Stack name (Extend x xs push))) a

newAdd :: StackModify 2 1 ()
newAdd = undefined

ixadd :: IxState (MyState (Stack name (x ': y ': xs))) (MyState (Stack name (k ': xs))) ()
ixadd = undefined

(>>>>) :: IxState i o a -> IxState o j b -> IxState i j b
a >>>> b = a >>>= \_ -> b
infixl 3 >>>>

data MyState stack = MyState
  { _bytecode :: String
  }

comp :: IxState (MyState EmptyStack) (MyState EmptyStack) ()
comp = do
  ixpush (5 :: Integer)
  ixpush (3 :: Integer)
  ixop (12 :: Integer) -- Arbitrary runtime computation inside MyState
  newAdd
  ixpop
  where (>>) = (>>>>)

-- wow :: IO ()
-- wow = do
--   stack1 <- push (undefined :: EmptyStack) (3 :: Int)
--   stack2 <- push stack1 (4 :: Int)
--   (_, stack3) <- pop (stack2)
--   (_, stack4) <- pop stack3
--   pure ()
