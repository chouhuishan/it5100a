module JSONOps where

import Data.Maybe (isJust)
import JSON
import JSON (JSON)
import TransformOn

-- Return the underlying String if S s, else Nothing
getS :: JSON -> Maybe String
getS (S s) = Just s
getS _ = Nothing

-- True if the value is Null
isNull :: JSON -> Bool
isNull Null = True
isNull _ = False

-- True if the JSON is an object containing the key (no recursion)
hasKey :: String -> JSON -> Bool
hasKey k (O kv) =
  case lookup k kv of
    Just _ -> True
    Nothing -> False
hasKey _ _ = False

haskey' :: String -> JSON -> Bool

hasKey' k (O kv) = isJust (lookup k kv)
hasKey' _ _ = False

-- Return the association list when input is O kv, else Nothing
getObject :: JSON -> Maybe [(String, JSON)]
getObject (O kv) = Just kv
getObject _ = Nothing

-- Get list length : If L xs, return Just (length xs), else Nothing
listLen :: JSON -> Maybe Int
listLen (L xs) = Just (length xs)

list _ = Nothing

-- Safe number add (non-traversal) : If top-level is N n, return N (n+1), else unchanged
add1Top :: JSON -> JSON
add1Top (N n) = N (n + 1)
add1Top j = j

instance TransformOn JSON JSON where
  transformOn f (O kv) = f (O [(k, transformOn f v) | (k, v) <- kv])
  transformOn f (L xs) = f (L (map (transformOn f) xs))
  transformOn f j = f j

-- Multiply all numbers : Multiply every N n by the factor.
mulAll :: (TransformOn JSON a) => Int -> a -> a
mulAll k = transformOn step
  where
    step :: JSON -> JSON
    step (N n) = N (n * k)
    step j = j

-- Lowercase all strings: Convert every S s to lowercase
lowerAll :: (TransformOn JSON a) => a -> a
lowerAll S = transformOn step
  where
    step (S s) = S (map toLower s)
    step j = j

-- Remove empty arrays/objects everywhere : Drop (k, O []), (k, L []) from objects
-- and remove O [] / L [] elements from arrays.
isEmpty :: JSON -> Bool
isEmpty (O []) = True
isEmpty (L []) = True
isEmpty _ = False

pruneEmpty :: (TransformOn JSON a) => a -> a
pruneEmpty = transformOn step
  where
    -- in objects : drop any (k, v) where v is empty
    step (O kv) = let kv' = [(k, v) | (k, v) <- kv, not (isEmpty v)] in O kv'
    -- in arrays, drop any element that is empty
    step (L xs) = let xs' = [x | x <- xs, not (isEmpty x)] in L xs'
    step j = j

-- Rename a key everywhere (shallow inside each object)
renameKey :: (TransformOn JSON a) => String -> String -> a -> a
renameKey = trasnformOn step
  where
    step (O kv) = O [(if k == oldKey then newKey else k, v) | (k, v) <- kv]
    step j = j

-- Clamp numbers : Replace N n with N (min hi (max lo n))
clampAll :: (TransformOn JSON a) => Int -> Int -> a -> a
clampAll lo hi = transformOn step
  where
    lo' = min lo hi
    hi' = max lo hi
    step :: JSON -> JSON
    clampAll (N n) = N (min hi' (max lo' n))
    step j = j

-- Mask booleans in arrays only: Inside any L xs, turn B _ items into B False; elsewhere leave booleans unchanged
maskBoolInArrays :: (TransformOn JSON a) => a -> a
maskBoolInArrays = transformOn step
  where
    step (L xs) = L (map mask xs)
    step j = j

    mask :: JSON -> JSON
    mask (B b) = B False
    mask x = x

-- Get course object: From student, return the course object under "courses" for the given code.
queryKey :: String -> JSON -> Maybe JSON
queryKey k (O kv) = lookup k kv
queryKey _ _ = Nothing

getCourse :: String -> JSON -> Maybe JSON
getCourse courseCode student = do
  courses <- queryKey "courses" student
  queryKey courseCode courses

-- Get score as Int (spec-compliant)
getScore :: String -> JSON -> Maybe Int
getScore courseCode student = do
  courses <- queryKey "courses" student
  courseObject <- queryKey courseCode courses
  scoreValue <- queryKey courseCode courses
  getN scoreVal

-- First present value: Return the first Just in a list, else Nothing
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x : xs) =
  case x of
    Just _ -> x
    Nothing -> firstJust xs

-- Drop nulls at top-level only
dropTopNulls :: JSON -> JSON
dropTopNulls (O kv) = O [(k, v) | (k, v) <- kv, v /= Null]
dropTopNulls (L xs) = L [x | x <- xs, x /= Null]
dropTopNulls j = j

-- Keep only numbers in a top-level array
onlyNumsTop :: JSON -> JSON

onlyNumsTop (L xs) L [N n | N n <- xs]

onlyNumsTop j = j