module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL gl fun) = GL (emp:gl) (fun + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend l (GL emp _) = foldr glCons l emp

moreFun :: GuestList -> GuestList -> GuestList
moreFun l r = if l > r then l else r


-- Did not understand the assignment here
treeFold' :: (a -> b -> b) -> b -> Tree a -> b
treeFold' f acc (Node x []) = f x acc
treeFold' f acc (Node x xs) = foldl (\a tr -> treeFold' f a tr) (f x acc) xs

sumTest = treeFold' (+) 0 (Node 2 [Node 3 [Node 4 []], Node 5 []])


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x []) = f x []
treeFold f (Node x xs) = f x (map (treeFold f) xs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gLists = (glCons boss $ foldl (\acc (_, gl) -> acc <> gl) mempty gLists
                        , foldl (\acc (gl, _) -> acc <> gl) mempty gLists)

maxFun :: Tree Employee -> GuestList
maxFun tr = case gl of
  (with, without) -> if with > without then with else without
  where gl = treeFold nextLevel tr


putLines :: [String] -> IO ()
putLines [] = return ()
putLines (x:xs) = do
  putStrLn x
  putLines xs

partyMain = do
  contents <- readFile "company.txt"
  let companyTree = read contents
      guestList = maxFun companyTree
  putStrLn $ "Total fun: " ++ show (fun guestList)
  putLines . sort $ map empName $ guests guestList
