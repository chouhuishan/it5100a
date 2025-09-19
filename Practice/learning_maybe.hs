-- Maybe is a type constructor, takes in type a, e.g. Maybe Int, Maybe String
-- Value constructor:
-- Nothing : no value/failure (no error info)
-- Just a : success -> x :: a

-- Bascially, it is a null-safe box, either it is Nothing (empty) or it contains exactly one value (Just x)
-- Forces you to handle both cases, so you dont crash with null
-- Allows function to signal failure without exceptions

import Data.Maybe (fromMaybe, maybe)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x -- (x:_) means any non-empty list and bind the first element to x and to ignore the rest

-- E.g.
-- safeHead []
-- Nothing

-- safeHead [1]
-- Just 1

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

-- E.g.
-- safeTail []
-- Nothing

-- safeTail [1, 2, 3]
-- Just [2,3]

use1 :: Maybe Int -> Int -- because of the Maybe Int, it doesnt have to be list unlike firstMessage
use1 mx = case mx of
  Nothing -> 0
  Just x -> x + 1

-- E.g.
-- use1 Nothing
-- 0

-- use1 (Just 1)
-- 2

use2 :: Maybe Int -> Int
use2 = fromMaybe 0 . fmap (+ 1) -- apply (+1) inside the Maybe box

-- E.g.
-- use2 Nothing
-- 0

-- use2 (Just 1)
-- 2

use2' mx = case mx of -- basically a rewrite of the above use2
  Nothing -> 0
  Just x -> x + 1

use3 :: Maybe Int -> Int -- this will excude the same behaviour as use2 written with maybe instead of fmap
use3 = maybe 0 (+ 1)

-- E.g.
-- use3 Nothing
-- 0

-- use3 (Just 1)
-- 2

firstMessage :: (Show a) => [a] -> String -- for any type a that has a show instance, and returns a string
firstMessage xs = case safeHead xs of
  Nothing -> "no head"
  Just x -> "head is " ++ show x

-- E.g.
-- firstMessage [1, 2, 3]
-- "head is 1"

-- firstMessage []
-- "no head"

firstMessage' :: [String] -> String -- when show is no used
firstMessage' [] = "no head"
firstMessage' (x : _) = "head is " ++ x

-- E.g.
-- firstMessage' []
-- "no head"

-- firstMessage' ["1"]
-- "head is 1"
