{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cenary.EvmAPI.API
  ( OpcodeM (..)
  , toOpcode
  , stop
  , add
  , mul
  , sub
  , div
  , mod
  , gt
  , lt
  , eq
  , iszero
  , pop
  , mload
  , mstore
  , mstore8
  , sload
  , sstore
  , jump
  , jumpi
  , codecopy
  , dup1
  , exp
  , calldataload
  , dup2
  , dup3
  , dup4
  , dup5
  , dup6
  , swap1
  , swap2
  , swap3
  , swap4
  , log0
  , log1
  , log2
  , op_return
  , address
  , push1
  , push4
  , push32
  , generateByteCode
  , inc
  , dec
  , sha3
  , leq
  , Instruction (JUMPDEST)
  , gen
  , mul'
  , add'
  , mstore'
  , pop'
  , jumpi'
  , iszero'
  , Instr (..)
  , ValueTy (..)
  , push32'
  , lt'
  , sub'
  , swap1'
  , unsafeGen
  , dup3'
  , inc'
  , stop'
  ) where

import           Data.Monoid ((<>))
import           Cenary.EvmAPI.Instruction (Instruction (..))
import           Cenary.EvmAPI.Program (Program (..))
import           Prelude hiding (EQ, GT, LT, div, exp, mod)
import           Text.Printf
import           Cenary.EvmAPI.OpcodeM
import           Cenary.EvmAPI.Instruction
import qualified Control.Category as Cat

data ValueTy = IntVal
             | Address ValueTy
             | Destination

newtype Instr (xs :: [ValueTy]) (ys :: [ValueTy]) = Instr
  { _unInstr :: [Instruction] -> [Instruction]
  }

{-
  And this is the essence to our API. We are propagating our
  stack state at type level by binding instructions, given
  that they tell us what kind of modification they do.
-}
instance Cat.Category Instr where

  (.) :: Instr b c -> Instr a b -> Instr a c
  Instr f1 . Instr f2 = Instr (f2 . f1)

  id :: Instr a a
  id = Instr id

stop, add, mul, sub, div, mod, gt, lt, eq, iszero, pop, mload, mstore, mstore8, sload, sstore, jump, jumpi, codecopy, dup1, exp, calldataload, dup2, dup3, dup4, dup5, dup6, swap1, swap2, swap3, swap4, log0, log1, log2, op_return, address, sha3 :: OpcodeM m => m ()

instr :: Instruction -> Instr a b
instr i = Instr $ (i:)

unsafeGen :: OpcodeM m => Instr xs ys -> m ()
unsafeGen = sequence_ . map op . ($ []) . _unInstr

gen :: OpcodeM m => Instr xs xs -> m ()
gen = sequence_ . map op . ($ []) . _unInstr

stop' :: Instr xs '[]
stop' = instr STOP

add' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
add' = instr ADD

mul' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
mul' = instr MUL

sub' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
sub' = instr SUB

div' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
div' = instr SUB

mod' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
mod' = instr SUB

gt' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
gt' = instr GT

lt' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
lt' = instr LT

eq' :: Instr ('IntVal ': 'IntVal ': xs) ('IntVal ': xs)
eq' = instr EQ

iszero' :: Instr ('IntVal ': xs) ('IntVal ': xs)
iszero' = instr ISZERO

pop' :: Instr xs xs
pop' = instr POP

mload' :: Instr xs xs
mload' = instr MLOAD

mstore' :: Instr xs xs
mstore' = instr MSTORE

mstore8' :: Instr xs xs
mstore8' = instr MSTORE8

sload' :: Instr xs xs
sload' = instr SLOAD

sstore' :: Instr xs xs
sstore' = instr SSTORE

jump' :: Instr xs xs
jump' = instr JUMP

jumpi' :: Instr ('Destination ': 'IntVal ': xs) xs
jumpi' = instr JUMPI

codecopy' :: Instr xs xs
codecopy' = instr CODECOPY

dup1' :: Instr xs xs
dup1' = instr DUP1

exp' :: Instr xs xs
exp' = instr EXP

calldataload' :: Instr xs xs
calldataload' = instr CALLDATALOAD

dup2' :: Instr xs xs
dup2' = instr DUP2

dup3' :: Instr xs xs
dup3' = instr DUP3

dup4' :: Instr xs xs
dup4' = instr DUP4

dup5' :: Instr xs xs
dup5' = instr DUP5

dup6' :: Instr xs xs
dup6' = instr DUP6

swap1' :: Instr (x ': y ': xs) (y ': x ': xs)
swap1' = instr SWAP1

swap2' :: Instr xs xs
swap2' = instr SWAP2

swap3' :: Instr xs xs
swap3' = instr SWAP3

swap4' :: Instr xs xs
swap4' = instr SWAP4

log0' :: Instr xs xs
log0' = instr LOG0

log1' :: Instr xs xs
log1' = instr LOG1

log2' :: Instr xs xs
log2' = instr LOG2

op_return' :: Instr xs xs
op_return' = instr RETURN

address' :: Instr xs xs
address' = instr ADDRESS

sha3' :: Instr xs xs
sha3' = instr SHA3

stop         = op STOP
add          = op ADD
mul          = op MUL
sub          = op SUB
div          = op DIV
mod          = op MOD
gt           = op GT
lt           = op LT
eq           = op EQ
iszero       = op ISZERO
pop          = op POP
mload        = op MLOAD
mstore       = op MSTORE
mstore8      = op MSTORE8
sload        = op SLOAD
sstore       = op SSTORE
jump         = op JUMP
jumpi        = op JUMPI
codecopy     = op CODECOPY
dup1         = op DUP1
exp          = op EXP
calldataload = op CALLDATALOAD
dup2         = op DUP2
dup3         = op DUP3
dup4         = op DUP4
dup5         = op DUP5
dup6         = op DUP6
swap1        = op SWAP1
swap2        = op SWAP2
swap3        = op SWAP3
swap4        = op SWAP4
log0         = op LOG0
log1         = op LOG1
log2         = op LOG2
op_return    = op RETURN
address      = op ADDRESS
sha3         = op SHA3

push1' :: forall x xs. Integer -> Instr xs (x ': xs)
push1' = instr . PUSH1

push1 :: OpcodeM m => Integer -> m ()
push1 = op . PUSH1

push4' :: forall x xs. Integer -> Instr xs (x ': xs)
push4' = instr . PUSH32

push4 :: OpcodeM m => Integer -> m ()
push4 = op . PUSH4

push32' :: forall x xs. Integer -> Instr xs (x ': xs)
push32' = instr . PUSH32

push32 :: OpcodeM m => Integer -> m ()
push32 = op . PUSH32

generateByteCode :: Program -> String
generateByteCode (Program instructions) =
  foldr (\instr bc -> bc <> to_bytecode instr) "" instructions
  where
    to_bytecode :: Instruction -> String
    to_bytecode instr =
      let (opcode, _) = toOpcode instr
       in (printf "%02x" (opcode :: Int)) <> (pushed_vals instr)

    pushed_vals :: Instruction -> String
    pushed_vals = \case
                  PUSH32 val -> printf "%064x" val
                  PUSH4 val -> printf "%08x" val
                  PUSH1 val -> printf "%02x" val
                  _          -> ""

inc :: OpcodeM m => Integer -> m ()
inc val = push32 val >> add

inc' :: Integer -> Instr ('IntVal ': xs) ('IntVal ': xs)
inc' val = push32' @'IntVal val Cat.>>> add'

dec :: OpcodeM m => Integer -> m ()
dec val = push32 val >> swap1 >> sub

leq :: OpcodeM m => m ()
leq = do
  push32 1
  swap1
  sub
  lt
