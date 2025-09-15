module Q2 (eqLast) where

eqLast :: Eq a => [a] -> [a] -> Bool
eqLast xs ys = not (null xs) && not (null ys) && last xs == last ys