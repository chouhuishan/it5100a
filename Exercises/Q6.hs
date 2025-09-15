module Q6 (sumDigits) where

sumDigits :: Integer -> Integer
sumDigits n
  | n < 10 = n
  | otherwise = (n `mod` 10) + sumDigits (n `div` 10)