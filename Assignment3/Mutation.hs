{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..),
    runOp, (>>>), (>~>), returnVal,
    StateOp(..), alloc, free
    )
    where

import AList (AList, lookupA, keyExistsA, insertA, updateA, removeA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer

-- ---------------------------------------------------------------------
-- Part 1: Altering the Mutable Class and creating instances for Part 1.

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Pointer a -> a -> StateOp ()

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Integer -> a -> StateOp (Pointer a)

-- Creating the integer and boolean instances

instance Mutable Integer where
    get (P val) = StateOp (\memo -> (
      (if keyExistsA val memo then
        case lookupA memo val of
          IntVal x -> x
       else
        error "Key not in memory"), memo))

    set (P pt) val = StateOp (\memo -> ((),
      if keyExistsA pt memo then
        updateA memo (pt, IntVal val)
      else
        error "Key not in memory"))

    def i val = StateOp (\memo ->
        if keyExistsA i memo then
            error "Key already in memory"
        else
            ((P i), insertA memo (i, IntVal val)))

instance Mutable Bool where
    get (P val) = StateOp (\memo -> (
      (if keyExistsA val memo then
        case lookupA memo val of
          BoolVal x -> x
       else
        error "Key not in memory"), memo))

    set (P pt) val = StateOp (\memo -> ((),
      if keyExistsA pt memo then
        updateA memo (pt, BoolVal val)
      else
        error "Key not in memory"))

    def i val = StateOp (\memo ->
      if keyExistsA i memo then
        error "Key already in memory"
      else
        ((P i), insertA memo (i, BoolVal val)))

-- ---------------------------------------------------------------
-- ---------------------------------------------------------------

-- Part 2: Chaining

-- State Operation
data StateOp a = StateOp (Memory -> (a, Memory))

-- This runs operaters in states.
runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- 'Then' Chaining Operation
(>>>) :: StateOp a -> StateOp b -> StateOp b
(>>>) f g = StateOp (\mem -> 
    let (_, mem2) = runOp f mem in runOp g mem2)

-- 'Bind' Chaining Operation
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
(>~>) f g = StateOp (\mem -> 
    let (x, mem2) = runOp f mem 
        newOp = g x 
    in runOp newOp mem2)

-- This function creates a new state operation.
returnVal :: a -> StateOp a
returnVal a = StateOp (\x -> (a, x))

-- ---------------------------------------------------------------
-- ---------------------------------------------------------------

-- Part 4: Safety Improvements

-- Finds space in memory.
findSpace val memo =
  if keyExistsA val memo then
    findSpace (val + 1) memo
  else
    val

-- This function automatically generates a new number to bind in memory.
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc item = StateOp (\memo ->
  runOp (def (findSpace 0 memo) item) memo)

-- Frees up a particular spot in memory.
free :: Mutable a => Pointer a -> StateOp ()
free (P ptr) = StateOp (\mem -> ((), removeA mem ptr))

-- ---------------------------------------------------------------

-- Given tests.
f :: Integer -> StateOp Bool
f x =
    def 1 4 >~> \p1 ->
    def 2 True >~> \p2 ->
    set p1 (x + 5) >>>
    get p1 >~> \y ->
    set p2 (y > 3) >>>
    get p2

g :: Integer -> StateOp Integer
g x =
    def 1 (x + 4) >~> \p ->
    get p >~> \y ->
    returnVal (x * y)