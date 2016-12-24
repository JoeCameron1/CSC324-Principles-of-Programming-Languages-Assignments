{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    keyExistsA,
    insertA,
    updateA,
    removeA
    )
    where


type AList a b = [(a, b)]

-- Part 0:

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key = let (a, b) = head alist in
    if (a == key) then
        b
    else
        lookupA (tail alist) key

-- ---------------------------------------------------------------------------

-- | Returns a boolean value.
--   Checks whether a key value already exists in alist or not.
--   Helper function for insertA.
keyExistsA key [] = False
keyExistsA key alist = let (a, b) = head alist in
    if (a == key) then
        True
    else
        keyExistsA key (tail alist)

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) =
    if (keyExistsA key alist) then
        alist
    else
        alist ++ [(key, val)]

-- ---------------------------------------------------------------------------

-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA alist (key, val) = map (\x -> let (a, b) = x in 
    if (a == key) then 
        (a, val) 
    else 
        (a, b)) alist

-- ---------------------------------------------------------------------------

-- | Returns a new association list which is the old one, except when
--   given a key, removeA removes a key value pair from the original alist.
--   If the key given is not found, removeA returns the original list.

removeA :: Eq a => AList a b -> a -> AList a b
removeA alist key = filter (\x -> let (k, v) = x in k /= key) alist


