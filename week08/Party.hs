-- CIS 194, week 8

module Party where

import Employee
import Data.Tree
import Data.Monoid
import Data.List

-- Exercise 1

-- 1. define glCons, which adds an Employee to GuestList

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

-- 2. define a Monoid instance for GuestList

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) $ f1 + f2

-- 3. define moreFun, which takes two GuestLists and returns the one
-- which has more fun

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

-- implement treeFold for Data.Tree

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a forest) = f a (map (treeFold f) forest)

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (withBoss, withoutBoss)
  where
    withoutBoss = mconcat $ map (uncurry moreFun) gls
    withBoss = glCons b $ mconcat $ map snd gls

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5

formatGLS :: GuestList -> String
formatGLS (GL es fun) =
  totalStr ++ "\n" ++ nameStr
  where
    totalStr = "Total fun: " ++ show fun
    nameStr = unlines $ sort $ map empName es

main :: IO ()
main = readFile "company.txt" >>= putStrLn . formatGLS . maxFun . read
