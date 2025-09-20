import Data.Either (rights)
import Text.Read (readMaybe)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

checkPositive :: Int -> Either String Int
checkPositive n
  | n > 0 = Right n
  | otherwise = Left "not positive"

safeDiv'' :: Int -> Int -> Either String Int -- NOTICE : previous codes only guards against 0 denominator. THIS IS FULLY SAFE-PROOF.
safeDiv'' a b
  | a <= 0 = Left "numerator cannot be 0"
  | b <= 0 = Left "demoninator cannot be 0"
  | otherwise = Right (a `div` b)

getIntByKey :: String -> [(String, String)] -> Maybe Int
getIntByKey k kvs = do
  -- input a key (k) String, and an association list kvs of (key, value) ; both Strings
  v <- lookup k kvs; readMaybe v -- lookup k kvs returns Maybe String
  -- if k is not in kvs -> Nothing
  -- if found, and the value is 42, it will return Just "42"

-- E.g.
-- :{
-- let kvs = [("x","42"), ("y","oops")]
-- :}
-- getIntByKey "x" kvs
-- Just 42

sumPos :: [Int] -> Either String Int
sumPos xs = do ys <- traverse checkPositive xs; pure (sum ys) -- pure == return
-- traverse : Map a function that returns an effect over a container,
--            combining the effects, and flip the structure.
-- in this context, traverse will apply checkPositive to each element of the list
-- and if any element fails, it will return Left "not positive" and stop
-- else Right (result)

-- E.g
-- sumPos [1, 2, 3]
-- Right 6

-- sumPos [-1, 2, 3]
-- Left "not positive"

(//?) :: Int -> Int -> Maybe Int
(//?) a b
  | a <= 0 = Nothing
  | b <= 0 = Nothing
  | otherwise = Just (a `div` b)

-- E.g
-- (//?) 6 0
-- Nothing

-- (//?) 0 6
-- Nothing

-- (//?) 6 3
-- Just 2

bounded :: Int -> Either String Int
bounded n
  | 0 <= n && n <= 100 = Right n
  | otherwise = Left "out of range"

percentOf :: Int -> Int -> Either String Int
percentOf part total = do
  p <- bounded part
  t <- checkPositive total
  safeDiv'' (p * 100) t

-- E.g.
-- percentOf 50 100
-- Right 50

-- explanation:
-- bounded 50 ; 0 <= n && n <= 100 --> Right 50 -> p = 50
-- checkPositive 100 ; 100 > 0 -> Right 100 -> t = 100
-- safeDiv'' (50 * 100) (100) = 50

-- percentOf (-50) (-1)
-- Left "out of range"

readInt :: IO (Either String Int)
readInt = do
  s <- getLine
  pure $ case reads s of
    [(n, "")] -> Right n
    _ -> Left "bad integer"

-- E.g.
-- readInt
-- 42
-- Right 42

-- readInt
-- 42abc
-- Left "bad integer"

-- readInt
-- []
-- Left "bad integer"

readInt' :: IO (Maybe Int)
readInt' = do
  s <- getLine
  pure $ case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

-- E.g.
-- readInt'
-- 42
-- Just 42

-- readInt'
-- 42abc
-- Nothing

-- readInt'
-- []
-- Nothing

main2 :: IO ()
main2 = do
  a <- readInt
  b <- readInt
  putStrLn $ either id show (do x <- a; y <- b; Right (x + y)) -- print the sum if both parses succeeded; otherwise print the error.

-- E.g.
-- main2
-- 42
-- 42
-- 84

-- 42abc
-- 42
-- bad integer

incMaybe :: Maybe Int -> Int
incMaybe = maybe 0 (+ 1)

-- E.g
-- incMaybe (Just 1)
-- 2

showEither :: Either String Int -> String
showEither = either ("error: " ++) (("value: " ++) . show)

-- E.g.
-- showEither (Right 7)
-- "value: 7"

-- showEither (Left "abc")
-- "error: abc"

-- ERROR : howEither (Left 7) , showEither (Right "abc")
-- Either String Int : Left value have to be String, while Right value must be an Int

showEither' :: Either Int String -> String
showEither' = either (("value: " ++) . show) ("error: " ++)

-- showEither' (Left 7)
-- "value: 7"

-- showEither' (Right "abc")
-- "error: abc"

collapse :: [Maybe a] -> Maybe [a]
collapse = sequence -- sequence here flips a list of Maybes into a Maybe list

-- E.g.
-- ghci> collapse [Just 1, Just 2, Just 3]
-- Just [1,2,3]

-- ghci> collapse [Just 1, Nothing, Just 3]
-- Nothing

-- ghci> collapse []
-- Just []

onlyRights :: [Either e a] -> [a]
onlyRights = rights

-- E.g.
-- rights [Right 1, Right 2, Right 3]
-- [1,2,3]