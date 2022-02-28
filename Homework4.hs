-- Name: Joseph Chang
-- NetID: G01189913

module Homework4 where

import Prelude hiding (zipWith,any)
--------------------------------------------------------------------------------

primeFactors :: Int -> [Int]
-- primeFactors starts with even numbers and divides until input is odd
-- uses div x y as integer division
primeFactors x
            | mod x 2 == 0  = 2:(primeFactors (div x 2))
            | otherwise     = primeFactorsNot2 3 x
-- iterates by 2 (keeping input odd) and keeps trying to find prime factors less than the input value
-- this recursive method is not as efficient as it can be for this method because I could not find a way to do integer square root
primeFactorsNot2 i x =  if i < x
                        then (if mod x i == 0 then i:(primeFactorsNot2 (i) (div x i)) else (primeFactorsNot2 (i+2) (x)))
                        else if x > 2 then x:[] else []

--------------------------------------------------------------------------------

coprime :: Int -> Int -> Bool
coprime x y = coprimeRec (primeFactors x) (primeFactors y)
-- No common factors were found and thus returns true
coprimeRec xs [] = True
-- Iterates through second param list and searches if it is in first param list
-- Uses elem to check if the current element (in second param list) is contained in first param list
coprimeRec xs (y:ys) = if elem y xs then False else coprimeRec xs ys

--------------------------------------------------------------------------------

trib :: Int -> Int
-- Starts off the list with 1 in index 2 (a) 1 (b) 0 (c) and the "index" var as x
trib x = tribRec 1 1 1 x
-- If the index var is 0, evaluate to the lowermost index value (c)
tribRec _ _ c 0 = c
-- Decrement x index and create next index (a+b+c)
tribRec a b c x = tribRec (a+b+c) a b (x-1)

--------------------------------------------------------------------------------

maxTwo :: [Int] -> [Int]
-- Base Cases
maxTwo [] = []
maxTwo (x:[]) = [x]
maxTwo (x:y:[]) = if x > y then x:y:[] else y:x:[]
-- Starter for recursion with the higher val of first two index elems as earlier parameter
maxTwo (x:y:xs) = if x > y then (maxTwoRec xs x y) else (maxTwoRec xs y x)
-- If there are no more in the list, return the known maxes
maxTwoRec [] y z = y:z:[]
-- Main recursive evaluation
maxTwoRec (x:xs) y z
                    | x > y = maxTwoRec xs x y -- new high max and second max is old highest
                    | x > z = maxTwoRec xs y x -- if new val not higher than max but is higher than second max
                    | otherwise = maxTwoRec xs y z -- next element in list

--------------------------------------------------------------------------------

reversed :: [a] -> [a]
-- If there are no more elements in the list, then return list terminal
reversed [] = []
-- put last in front and recurse with a list without last element (init xs)
reversed xs = (last xs):(reversed (init xs))

--------------------------------------------------------------------------------

clockwise :: [[Int]] -> [[Int]]
-- Edge cases when the input is empty
clockwise [] = []
clockwise [[]] = [[]]
-- Setup for the recursive clockwise function
-- Reverses the original 2d array then weaves them together
-- The init is because the empty clockwiseRec would add an empty list inside the end of the 2d array
clockwise xss = init (clockwiseRec (reversed xss))
-- Base case to finish recursion
clockwiseRec [] = []
clockwiseRec xss = [x | (x:xs) <- xss]:(clockwiseRec [xs | (x:xs) <- xss])
--clockwiseRec xss = [[ x | x <- xs] | xs <- (reversed xss)]

--------------------------------------------------------------------------------

any :: [Bool] -> Bool
-- No more elements means evaluate false
any [] = False
-- Iterate through list and if there is true, return true, otherwise keep iterating
any (x:xs) = if x == True then True else any xs

--------------------------------------------------------------------------------

select :: (a->Bool)-> [a] -> [a]
-- list comprehension: element is element from list only if func of x is true
select func xs = [x | x <- xs, func x == True]
                              
--------------------------------------------------------------------------------

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- Iterate through lists
-- For the head of return, func of x and y
-- For the tail of return, recurse with tail of lists
zipWith func (x:xs) (y:ys) = (func x y):(zipWith func xs ys)
-- Anything else means we exhausted either list
zipWith _ _ _ = []

--------------------------------------------------------------------------------

augdentity :: Int -> Int -> [[Int]]
augdentity x y = (augdentityRec x y 0 0):[]
augdentityRec x y i j
                    | j == i    = 1:(augdentityRec x y i (j+1))
                    | j < y     = 0:(augdentityRec x y i (j+1))
                    | i < x     = augdentityRec x y (i+1) 0
                    | otherwise = []

--------------------------------------------------------------------------------
