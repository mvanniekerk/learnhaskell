{-# LANGUAGE FlexibleInstances #-}
module Variables where

import qualified Data.Map.Lazy as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

instance HasVars VarExprT where
  var x = Var x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add f g = \m -> case (f m) of 
    Just x -> case (g m) of 
      Just y -> Just (x + y)
      Nothing -> Nothing
    Nothing -> Nothing
  mul f g = \m -> case (f m) of 
    Just x -> case (g m) of 
      Just y -> Just (x * y)
      Nothing -> Nothing
    Nothing -> Nothing

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

m = M.empty
