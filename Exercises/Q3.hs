module Q3 (isPalindrome) where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs