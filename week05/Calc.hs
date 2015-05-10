#! /usr/bin/env runhugs +l
--
-- Calc.hs
-- Copyright (C) 2015 zhao <zhao@kamel-ThinkPad-X201>
--
-- Distributed under terms of the MIT license.
--

module Calc where

import ExprT
import Parser
import Expr

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i | i <= 0 = False
        | i > 0  = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
                 deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer
                 deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul -- "(3 * -4) + 5"

testInteger = testExp :: String -> Maybe Integer
testBool    = testExp :: String -> Maybe Bool
testMM      = testExp :: String -> Maybe MinMax
testSat     = testExp :: String -> Maybe Mod7
