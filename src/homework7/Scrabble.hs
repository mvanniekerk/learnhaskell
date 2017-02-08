{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i 

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score 'b' = Score 3
score 'c' = Score 3
score 'd' = Score 2
score 'f' = Score 4
score 'g' = Score 2
score 'h' = Score 4
score 'j' = Score 8
score 'k' = Score 5
score 'm' = Score 3
score 'p' = Score 3
score 'q' = Score 10
score 'v' = Score 4
score 'w' = Score 4
score 'x' = Score 8
score 'y' = Score 4
score 'z' = Score 10
score l   = if isAsciiLower l then Score 1 else Score 0

scoreString :: String -> Score
scoreString = foldl (\a s -> a <> score s) mempty

