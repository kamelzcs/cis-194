-- CIS 194, week 8

module Party where

import Employee
import Data.Tree
import Data.Monoid

-- Exercise 1

-- 1. define glCons, which adds an Employee to GuestList

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

-- 2. define a Monoid instance for GuestList

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend g1@(GL es1 f1) g2@(GL es2 f2) = GL (es1 ++ es2) $ f1 + f2

-- 3. define moreFun, which takes two GuestLists and returns the one
-- which has more fun

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

-- implement treeFold for Data.Tree

treeFold :: (a-> b -> b) -> b -> Tree a -> b
treeFold f z t = foldr f z $ nodeList t

nodeList :: Tree a -> [a]
nodeList (Node a []) = [a]
nodeList (Node a sf) = a : (concat . map nodeList) sf

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> [(GuestList, GuestList)]
