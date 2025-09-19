-- Good practice : When writing functions, give the function an explicit type declaration

removeNonUpperCase :: [Char] -> [Char] -- removeNonUpperCase maps from a string to a string, takes a string as a parameter and return another string as a result
removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int -- Int -> (Int -> (Int -> Int)) ; takes an Int and returns a function that takes an Int, and returns a function that takes an Int and returns an Int
addThree x y z = x + y + z

-- Function with type variables are polymorphic functions
--