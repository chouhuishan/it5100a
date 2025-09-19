-- Either : Essentially a "Maybe" with an error message
-- Right x = success value
-- Left error = Failure with an explanation

checkPositive :: Int -> Either String Int
checkPositive n
  | n > 0 = Right n
  | otherwise = Left "not positive"

-- E.g.
-- checkPositive 5
-- Right 5

-- checkPositive (-3)
-- Left "not positive"

showCheck :: Int -> String
showCheck = either id (\n -> "ok: " ++ show n) . checkPositive

-- E.g
-- showCheck 7
-- "ok: 7"

-- showCheck 0
-- "not positive"

-- ghci> showCheck (-3)
-- "not positive"

-- QNS: What is id? It is an identity function.
-- e :: Either String Int
-- Left : String error -> keep it unchanged (id)
-- checkPositive x yields Left "not positive",
-- then id "not positive" is just "not positive".
-- That’s what “apply id to the error string … keep it unchanged”
-- Right : Turn into string and show n

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "division by 0 is error"
safeDiv a b = Right (a `div` b)

pipeline :: Int -> Int -> Either String Int
pipeline a b = do
  x <- checkPositive a
  y <- checkPositive b
  safeDiv x y

-- E.g.
-- safeDiv 3 0
-- Left "division by 0 is error"

-- safeDiv 0 3
-- Right 0

-- safeDiv 3 2
-- Right 1 -> IF YOU WANT 1.5, SEE safeDiv'

safeDiv' :: Int -> Int -> Either String Double
safeDiv' _ 0 = Left "division by 0 is error"
safeDiv' a b = Right (fromIntegral a / fromIntegral b) -- if you want float, use fromIntegral
-- safeDiv' a b = Right (a / b)   -- this should be a type error, because it does not return an Int

-- E.g.
-- safeDiv' 3 2
-- Right 1.5
