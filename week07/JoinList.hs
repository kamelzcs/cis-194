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

-- safe list indexing function

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- convert given JoinList to list, ignoring the monoidal annotations

jlToList :: JoinList m a -> [a]
jlToList Empty                 = []
jlToList (Single _ a)          = [a]
jlToList (Append _ left right) = jlToList left ++ jlToList right

-- return the monoidal annotation of the root of given JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- return the "size" of the JoinList
sz :: (Sized b, Monoid b) => JoinList b a -> Int
sz = getSize . size . tag

-- Exercise 1
-- Write an append function for JoinLists that yields a new JoinList
-- whose monoidal annotation is derived from those of the two
-- arguments

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 `mappend` tag l2) l1 l2

-- Exercise 2
-- 1. Implement the function indexJ to find the JoinList element at
-- the specified index; it should satisfy the equivalence:
--     (indexJ i jl) == (jlToList jl !!? i)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ _) | i > 0 = Nothing
indexJ _ (Single _ a) = Just a
indexJ i p@(Append _ l r)
  | i >= sz p = Nothing
  | i < lsize = indexJ i l
  | otherwise = indexJ (i - lsize) r
  where lsize = sz l

-- 2. Implement the function dropJ to drop first n elements of a
-- JoinList; it should satisfy the equivalence:
--     jlToList (dropJ n jl) = drop n (jlToList jl)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a ->JoinList b a
dropJ _ Empty        = Empty
dropJ n l | n < 0 = l
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r)
  | n < lsize = (dropJ n l) +++ r
  | otherwise = dropJ (n - lsize) r
  where lsize = sz l

-- 3. Implement the function takeJ to return the first n elements of a
-- JoinList, dropping all other elements; it should satisfy the equivalence:
--     jlToList (takeJ n jl) == take n (jlToList jl)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a ->JoinList b a
takeJ _ Empty          = Empty
takeJ n _  | n <= 0    = Empty
takeJ _ Empty = Empty
takeJ n j | n >= sz j = j
takeJ n (Append _ l r)
  | n < lsize = takeJ n l
  | otherwise = l +++ takeJ (n - lsize) r
  where lsize = sz l

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

type JLBuffer = JoinList (Score, Size) String

instance Buffer JLBuffer where

  -- toString :: JLBuffer -> String
  toString = concat . jlToList

  -- fromString :: String -> JLBuffer
  fromString = foldr1 (+++) . map(\x -> Single (scoreString x, Size 1) x) . lines

  -- line :: Int -> JLBuffer -> Maybe String
  line = indexJ

  -- replaceLine :: Int -> String -> JLBuffer -> JLBuffer
  replaceLine n str jlb =
    takeJ n jlb +++ Single (scoreString str, Size 1) str +++ dropJ (n + 1) jlb

  -- numLines :: JLBuffer -> Int
  numLines = sz

  -- value :: JLBuffer -> Int
  value = scorev . fst . tag
          where scorev (Score i) = i

-- JLBuffer based editor

main = runEditor editor jlb
  where jlb = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JLBuffer
