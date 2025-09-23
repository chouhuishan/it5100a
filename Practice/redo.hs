module JSONOps where

import JSON
import TransformOn

instance TransformOn JSON JSON where
  transformOn f (O kv) = f (O [(k, transformOn f v) | (k, v) <- kv])
  -- Pattern match : This clause matches JSON objects of the form O kv,
  -- whereby, kv :: [(String, JSON)]
  -- For each (k, v) pair in the object, it keeps the key (k) unchanged
  -- but recursively transform the value (v) by calling transformOn f v
  -- after all the values are recursively transformed,
  -- it reconstructs the object as (O [(k, transformOn f v) | (k, v) <- kv])
  -- Then it will apply f to this node
  transformOn f (L xs) = f (L (map (transformOn f) xs))
  -- Pattern match: This clause matches arrays L xs, where xs :: [JSON]
  -- tranformOn f is map over each element x of the list,
  -- so that each item is recursively transformed
  transformOn f j = f j

-- This clause matches all the remaining constructors
--  N Int, S String, B Bool, Null -- do not contain JSON values
-- therefore, nothing to recurse

addAll :: (TransformOn JSON a) => Int -> a -> a
addAll k = transformOn step -- TODO: Question 2
  where
    step :: JSON -> JSON
    step (N n) = N (n + k)
    step j = j

-- step is per-node trnasformer
-- if the node is a number N n, add k
-- else, leave the node unchanged

negateAll :: (TransformOn JSON a) => a -> a
negateAll = transformOn step -- TODO: Question 2
  where
    step (B b) = B (not b)
    step j = j

-- At any B b, it is replaced wth B (not b)

filterNull :: (TransformOn JSON a) => a -> a
filterNull = transformOn step -- TODO: Question 2
  where
    step (O kv) = O [(k, v) | (k, v) <- kv, v /= Null]
    step (L xs) = L [x | x <- xs, x /= Null]
    step j = j

-- Objects (0 kv) : Keep only pairs whose value v is not Null
-- Arrays (L xs) : keep only elements x that are not null

getN :: JSON -> Maybe Int
getN (N n) = Just n -- TODO: Question 3
-- Pattern match on the JSON value
-- if it is the N constructor, bind the integer to n -> Just n
getN _ = Nothing

-- Catch all for the other constructors : O [String, JSON], L [JSON], S String, B Bool, Null
-- Since those arent numbers, there is no integer to return -> Nothing

queryKey :: String -> JSON -> Maybe JSON
queryKey k (O kv) = lookup k kv -- TODO: Question 4
-- this clause applies when JSON is an object built with O constructor
-- kv :: [(String, JSON)] is the list of key-value pairs
-- lookup is the prelude function with type lookup :: Eq a => a -> [(a, b)] -> Maybe b 
-- lookup :: String -> [(String, JSON)] -> Maybe JSON
-- it scans kv for the first pair whose key equals k 
-- if found, return Just value; otherwise Nothing
queryKey _ _ = Nothing
-- catch all clause for any JSON that is not an object (arrays L numbers N, Strings S, Boolean B, Null)

getScore :: String -> JSON -> Maybe Int
getScore courseCode student = do
    -- In the Maybe monad, each bind (<-) means: 
    -- “if the expression on the right is Just x, bind x; 
    -- if it’s Nothing, stop here and return Nothing.”
  courses <- queryKey "courses" student
  -- look up "courses" in the student JSON
  -- if student JSON is an object that has "courses" key, courses is bound to that JSON value
    -- if missing or student is not an object, the function return Nothing
  value <- queryKey courseCode courses
  -- look up specific courseCode inside courses
  -- if the course exist, value becojmes that course's JSON object
  getN value
  -- Extract an Int if value is a JSON number(N n)

