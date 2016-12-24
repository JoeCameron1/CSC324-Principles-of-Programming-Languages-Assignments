module CompoundMutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..), -- Part 0 and Part 1
    runOp, (>>>), (>~>), returnVal, -- Part 2
    StateOp(..), alloc, free -- Part 4
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
data Pointer a = P Integer | P2 Integer Integer deriving Show -- Changed for Part 5

data Person = Person Integer Bool deriving Show

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

-- State Operation.
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
-- Part 5: Compound Mutable Values

-- Returns the correct pointer.
(@@) :: Pointer a -> Integer -> Pointer b
personPointer @@ attribute =
  let (P2 p1 p2) = personPointer in
  if (attribute == 1) then
    (P p1)
  else if (attribute == 2) then
    (P p2)
    else
      error "This attribute does not correspond."

-- Age represented as a function.
age :: Integer
age = 1

-- Student represented as a function.
isStudent :: Integer
isStudent = 2


instance Mutable Person where
  get (P2 first_attribute second_attribute) = StateOp (\memo ->
    let p1 = (P first_attribute) :: Pointer Integer
        p2 = (P second_attribute) :: Pointer Bool
        (age, memo1) = runOp (get (p1 :: Pointer Integer)) memo
        (bool, memo2) = runOp (get (p2 :: Pointer Bool)) memo
    in ((Person age bool), memo))

  set (P2 first_attribute second_attribute) person =
    let (Person age bool) = person
    in set (P first_attribute) age >>> set (P second_attribute) bool

  def i (Person age stud) = StateOp (\memo ->
    let ((P num), val) = runOp (def i age >>> alloc stud) memo
    in (P2 i num, val))

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

-- Given test for Person.

personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x =
    -- not using alloc, but we could
    def 1 person >~> \personPointer ->
    get (personPointer @@ age) >~> \oldAge ->
    set (personPointer @@ age) x >>>
    get (personPointer @@ isStudent) >~> \stu ->
    get (personPointer @@ age) >~> \newAge ->
    set personPointer (Person (2 * newAge) (not stu)) >>>
    get personPointer >~> \newPerson ->
    get (personPointer @@ isStudent) >~> \newStu ->
    returnVal (oldAge, newStu, newPerson)

