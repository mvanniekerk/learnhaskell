{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)
  
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs) 

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c 
streamZipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (streamZipWith f xs ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

--1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
--0 1 0 2 0 1 0 3 0 1  0  2  0  1  0  4  0  1  0  2  0  1  0  3  0  1  0  2  0  1  0  5

interLeaveStreams :: Stream a -> Stream a -> Stream a
interLeaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interLeaveStreams xs ys))

--Works until 2^101 (which is pretty big)
ruler :: Stream Integer
ruler = rule 0

rule :: Integer -> Stream Integer
rule 100 = streamRepeat 100
rule n = interLeaveStreams (streamRepeat n) (rule (n+1))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0)) 

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (negate)
  (+) = streamZipWith (+)
  (*) (Cons a aa) bs@(Cons b bb) = Cons (a*b) ((streamMap (*a) bb) + (aa * bs))

instance Fractional (Stream Integer) where
  (/) a1@(Cons a aa) b1@(Cons b bb) = Cons (a `div` b) (streamMap (`div` b) (aa - (a1/b1) * bb))

fibs3 :: Stream Integer
fibs3 = x/(1-x-x^2)

data Matrix = Matrix Integer Integer Integer Integer
  deriving (Show)

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
           (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 n = case m of
  Matrix _ x _ _  -> x
  where m = (Matrix 1 1 1 0) ^ n
