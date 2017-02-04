{-# LANGUAGE FlexibleInstances #-}
module Stack where

import StackVM
import Parser
import Data.Maybe

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul


testProgram = stackVM (fromJust (compile "(3*-4) + 5"))



