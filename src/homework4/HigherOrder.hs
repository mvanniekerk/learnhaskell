module HigherOrder where

import Data.List ((\\), foldl')


fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x-2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl' (\t x -> (x-2) * t) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3*x+1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x (Node i left y right)
  | hl < hr = Node i inLeft y right
  | hl > hr = Node i left y inRight
  -- Just to get the count right, not necessary for the algorithm
  | (height inLeft) == hl = Node i inLeft y right
  | otherwise = Node (i+1) inLeft y right
  where hl = height left
        hr = height right
        inLeft = insert x left
        inRight = insert x right
insert x Leaf = Node 0 Leaf x Leaf

height :: Tree a -> Integer
height tr = case tr of
  Leaf -> -1
  Node i _ _ _ -> i

xor :: [Bool] -> Bool
xor = foldl' (\a c -> if c then not a else a)False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\c a -> f c : a) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\n -> n*2 + 1) . sieve

sieve :: Integer -> [Integer]
sieve n = [1..n] \\ [i+j+2*i*j | i <- [1..n], j <- [1..n], i<=j, i+j+2*i*j<=n]
