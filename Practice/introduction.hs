doubleMe x = x + x

-- doubleUs x y = x * 2 + y * 2

-- redefining doubleUs x y ; combine basic functions and combine into complex functions
doubleUs x y = doubleMe x + doubleMe y

-- Introduction of "if" statement
-- difference from imperative languages: "else" part is mandatory in Haskell
-- "if" statement is an expression in Haskell because "else" is mandatory -> it will always return something
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1 -- it is correct to type doubleSmallNumber in 1 sentence but the above code is more readable

-- list comprehension : filter
-- x <- xs : iterate over element x of the input list xs
boomBang xs = [if x < 10 then "Boom!" else "BANG!" | x <- xs, odd x] -- expected output: boomBang [7..13] -> ["Boom!","Boom!","BANG!","BANG!"]

double xs = [x * 2 | x <- xs] -- expected output : double [1..10] -> [2,4,6,8,10,12,14,16,18,20]

doubleFilter xs = [x * 2 | x <- xs, x * 2 >= 12] -- expected output : doublefilter [1..10] -> [12,14,16,18,20]

learningMod xs = [x | x <- xs, x `mod` 7 == 3] -- expected output : learningMod [50..100] -> [52,59,66,73,80,87,94]
-- mod : calculates the remainder of an integer division

learningFilter xs = [x | x <- xs, x /= 13, x /= 15, x /= 19] -- expected output : learningFilter [10..20] -> [10,11,12,14,16,17,18,20]

-- list comprehension : product
pairOfProduct xs ys = [x * y | x <- xs, y <- ys] -- expected output : pairOfProduct [2, 5, 10] [8, 10, 11] -> [16,20,22,40,50,55,80,100,110]

pairOfProduct' xs ys = [x * y | x <- xs, y <- ys, x * y > 50] -- expected output : pairOfProduct [2, 5, 10] [8, 10, 11] -> [55,80,100,110]

length' xs = sum [1 | _ <- xs] -- _ means that we do not care what we will draw from the lisst
-- [1 | _ <- xs] means “for every element in xs, ignore it (_) and yield 1”, so for ten elements you get [1,1,1,1,1,1,1,1,1,1]
-- expected output : length' [1..10] -> 10

removeLowercase st = [c | c <- st, c `elem` ['A' .. 'Z']] -- elem returns True if the list contain an item == to the first argument
-- this function basically removes all the lower caps and return the uppercaps
-- takes each character c from st and keep only if c is in the list [A..Z] and collect the kept characters into a new list
-- expected output : removeLowercase "Hahaha! Ahahaha!" -> "HA"

removeOddNumbers xss = [[x | x <- xs, even x] | xs <- xss] -- expected outcome : removeOddNumbers [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]] -> [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]

-- tuples : zip function
pairUp xs ys = zip xs ys -- expected outcome : pairUp [1,2,3,4,5] [5,5,5,5,5] -> [(1,5),(2,5),(3,5),(4,5),(5,5)]
-- expected outcome : pairUp [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"] -> [(5,"im"),(3,"a"),(2,"turtle")]
-- note that the example above that zip can take two lists that contain different types and zip them up
-- QUESTION : WHAT HAPPENS IF THE LENGTH OF THE LIST DO NOT MATCH?
-- The longer list is cut short to match the length of the shorter one.

-- Combination of list comprehension & tuples
triangles t = [(a, b, c) | c <- t, b <- t, a <- t] -- expected outcome : triangles [1..10] -> VERY LONG, lists all possible tuple combination

rightTriangles t = [(a, b, c) | c <- t, b <- t, a <- t, a ^ 2 + b ^ 2 == c ^ 2] -- expected outcome : rightTriangles [1..10] -> [(4,3,5),(3,4,5),(8,6,10),(6,8,10)]

rightTriangles' t = [(a, b, c) | c <- t, b <- t, a <- t, a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24] -- rightTriangles' [1..10] -> [(8,6,10),(6,8,10)]

-- HASKELL BASIC
-- 1) ++ operator : concatenation
-- [1, 2, 3, 4] ++ [9, 10, 11, 12] = [1,2,3,4,9,10,11,12]
-- "hello" ++ " " ++ "world" = "hello world"
-- ['w','o'] ++ ['o','t'] = "woot"

-- 2) : operator : puts something in front of the list
-- "A" : " SMALL CAT" -> "A SMALL CAT"
-- 1:[2, 3, 4, 5] -> [1, 2, 3, 4, 5]

-- 3) !! : Get an element out of the list by index; indices start at 0
-- "Steve Buscemi" !! 6  -> 'B'
-- [9.4,33.2,96.2,11.2,23.25] !! 1 -> 33.2

-- 4) head : takes a list and return the first element (index = 0)
-- 5) tail : takes a list and returns the everything except the head
-- 6) last : takes a list and return the last element
-- 7) length : equivalent of len()
-- 8) null : checks if the list is empty --> if empty, return True, esle, return False
-- 9) reverse : reverses a list ; reverse [5, 4, 3, 2, 1] --> [1, 2, 3, 4, 5]

-- 10) take : takes a number and returns the first n elements of the list
-- take 3 [5,4,3,2,1] -> [5,4,3] ; (see below for explanation)

-- take 3 [5,4,3,2,1]
-- = 5 : take 2 [4,3,2,1]
-- = 5 : (4 : take 1 [3,2,1])
-- = 5 : (4 : (3 : take 0 [2,1]))
-- = 5 : (4 : (3 : []))
-- = [5,4,3]

-- take 1 [3,9,3] -> [3]
-- take 5 [1,2] -> [1,2]
-- take 0 [6, 6, 6] -> [6, 6, 6] -> NOTE: if we try to take 0 elements, we get an empty list

-- 11) drop : skips the first n elements of the list and return the rest
-- drop 3 [8,4,2,1,5,6] -> [1, 5, 6] ; (see below for explanation)

-- drop 3 [8,4,2,1,5,6]
-- = drop 2 [4,2,1,5,6]   -- skip 8
-- = drop 1 [2,1,5,6]     -- skip 4
-- = drop 0 [1,5,6]       -- skip 2
-- = [1,5,6]

-- drop 0 [1,2,3,4] -> [1,2,3,4]
-- drop 100 [1,2,3,4] -> []

-- 12) maximum : return the largest element
-- 13) minimum : return the smallest element

-- 14) elem : returns True if the list contains an item equal to the first argument
-- 4 `elem` [3, 4, 5, 6] --> True
-- 10 `elem` [3, 4, 5, 6] --> False

-- 15) Texas Ranges
-- [1..20] -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- ['a' .. 'z'] -> "abcdefghijklmnopqrstuvwxyz"

-- 15.1) Texas ranges w step
-- [2,4..20] -> [2,4,6,8,10,12,14,16,18,20]
-- [3,6..20] -> [3,6,9,12,15,18]

-- Try to avoid using floating numbers in ranges as they are not completely precise
-- [0.1, 0.3 .. 1] -> [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]

-- 16) cycle : takes a list and cycles into infinite cycle
-- take 10 (cycle [1,2,3]) -> [1,2,3,1,2,3,1,2,3,1]
-- take 12 (cycle "LOL ") -> "LOL LOL LOL "

-- 17) repeat : take an element and produce an infinite list of just that element
-- take 10 (repeat 5) : [5,5,5,5,5,5,5,5,5,5]