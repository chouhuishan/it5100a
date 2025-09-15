module JSONOps where

import JSON
import TransformOn

-- TODO: Question 1
instance TransformOn JSON JSON

addAll :: (TransformOn JSON a) => Int -> a -> a
addAll = undefined -- TODO: Question 2

negateAll :: (TransformOn JSON a) => a -> a
negateAll = undefined -- TODO: Question 2

filterNull :: (TransformOn JSON a) => a -> a
filterNull = undefined -- TODO: Question 2

getN :: JSON -> Maybe Int
getN = undefined -- TODO: Question 3

queryKey :: String -> JSON -> Maybe JSON
queryKey = undefined -- TODO: Question 4

getScore :: String -> JSON -> Maybe Int
getScore = undefined -- TODO: Question 5
