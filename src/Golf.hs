module Golf where

import Data.List

skips :: [a] -> [[a]]
skips m = map (\(n,xs) -> skip n xs) (zip [1..] (replicate (length m) m))

skip :: Int -> [a] -> [a]
skip n xs = case drop (n-1) xs of
  (y:ys) -> y : skip n ys
  [] -> []

localMaxima :: [Int] -> [Int]
localMaxima n = case n of
  (a:x@(b:c:_)) -> if a < b && b > c then b:localMaxima x else localMaxima x
  _ -> []

histogram :: [Int] -> String
histogram x = 
  (unlines 
    (reverse 
      (transpose 
        (map 
          (\n -> (replicate n '*') ++ (replicate (length x-n) ' ')) 
          (count x))))) ++ "----------\n0123456789\n"

count :: [Int] -> [Int]
count a = [foldl (\c e -> if e == x then c+1 else c) 0 a | x <- [0..9]]
