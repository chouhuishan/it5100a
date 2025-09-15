module Q5 (burgerPrice) where

burgerPrice :: String -> Double
burgerPrice ingredient = sum (map ingredientPrice ingredient)
  where
    ingredientPrice :: Char -> Double
    ingredientPrice ch = case ch of
      'B' -> 0.50
      'C' -> 0.80
      'P' -> 1.50
      'V' -> 0.70
      'O' -> 0.40
      'M' -> 0.90
      _ -> 0.0