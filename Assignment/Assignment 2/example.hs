class TransformOn a b where
  transformOn :: (a -> a) -> b -> b

instance TransformOn Int Int where
  transformOn :: (Int -> Int) -> Int -> Int
  transformOn f x = f x

instance (TransformOn Int a, TransformOn Int b) => TransformOn Int (a, b) where
  transformOn :: (Int -> Int) -> (a, b) -> (a, b)
  transformOn f (x, y) = (transformOn f x, transformOn f y)

addDeeply :: (TransformOn Int a) => Int -> a -> a
addDeeply k = transformOn (+ k)
