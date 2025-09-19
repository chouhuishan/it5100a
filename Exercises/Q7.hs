module Q6 (slice) where

slice :: [a] -> (Int, Int) -> [a]
slice xs (start, stop) = take (stop - start) (drop start xs)