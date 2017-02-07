{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                 | Single m a
                 | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl = case jl of
  Empty -> Nothing
  (Single _ a) -> Just a
  (Append _ l r) -> 
    if i < tl then indexJ i l else indexJ (i+tr) r
    where tl = getSize . size $ tag l
          tr = getSize . size $ tag r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i (Append t l r) 
  | i > getSize (size t) = Empty
  | i == tl = r 
  | i <  tl = (dropJ i l) +++ r 
  | i >  tl = dropJ (i-tl) r
  where tl = getSize . size $ tag l
        tr = getSize . size $ tag r
dropJ i jl = jl
        
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i (Append _ l r)
  | i < 0 = Empty
  | i == tl = l
  | i >  tl = l +++ (takeJ i r)
  | i <  tl = takeJ i l
  where tl = getSize . size $ tag l
        tr = getSize . size $ tag r
takeJ i jl = jl

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ w) = w
  toString (Append _ l r) = toString l ++ toString r
  fromString s = foldl (+++) Empty . map scoreSizeLine $ lines s
  line = indexJ
  replaceLine i s b = takeJ i b +++ scoreSizeLine s +++ dropJ (i+1) b
  numLines = getSize . size . snd . tag 
  value = getScore . fst . tag

myBuffer :: JoinList (Score, Size) String
myBuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

replaceTest = replaceLine 2 "yeah not today" myBuffer

jlMain = runEditor editor myBuffer

