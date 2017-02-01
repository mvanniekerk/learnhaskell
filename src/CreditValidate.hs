module CreditValidate
    (validate)
    where

import Data.Char

toDigits :: Int -> [Int]
toDigits i = map (digitToInt) (show i)

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits 

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = reverse . deoRev . reverse

deoRev :: [Int] -> [Int]
deoRev li = case li of
           [] -> []
           (x:y:xs) -> x : (y*2) : deoRev xs
           x -> x

sumDigits :: [Int] -> Int
sumDigits = sum . map (sum . toDigits)

validate :: Int -> Bool
validate = (\n -> n `mod` 10 == 0) . sumDigits . doubleEveryOther . toDigits 
