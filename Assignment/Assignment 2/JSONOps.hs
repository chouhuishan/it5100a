module JSONOps where

import JSON
import TransformOn

-- TODO: Question 1
instance TransformOn JSON JSON where
  transformOn f (O kv) = f (O [(k, transformOn f v) | (k, v) <- kv])
  transformOn f (L xs) = f (L (map (transformOn f) xs))
  transformOn f j = f j

addAll :: (TransformOn JSON a) => Int -> a -> a
addAll k = transformOn step -- TODO: Question 2
  where
    step :: JSON -> JSON
    step (N n) = N (n + k)
    step j = j

negateAll :: (TransformOn JSON a) => a -> a
negateAll = transformOn step -- TODO: Question 2
  where
    step (B b) = B (not b)
    step j = j

filterNull :: (TransformOn JSON a) => a -> a
filterNull = transformOn step -- TODO: Question 2
  where
    step (O kv) = O [(k, v) | (k, v) <- kv, v /= Null]
    step (L xs) = L [x | x <- xs, x /= Null]
    step j = j

getN :: JSON -> Maybe Int
getN (N n) = Just n -- TODO: Question 3
getN _ = Nothing

queryKey :: String -> JSON -> Maybe JSON
queryKey k (O kv) = lookup k kv -- TODO: Question 4
queryKey _ _ = Nothing

getScore :: String -> JSON -> Maybe Int
getScore courseCode student = do
  -- TODO: Question 5
  courses <- queryKey "courses" student
  value <- queryKey courseCode courses
  getN value
