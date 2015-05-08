#! /usr/bin/env runhugs +l
--
-- Golf.hs
-- Copyright (C) 2015 zhao <zhao@kamel-Desktop>
--
-- Distributed under terms of the MIT license.
--


module Golf where

import Data.List

-- Exercise 1

skips :: [a] -> [[a]]
skips xs =
        let ns = [1..length xs]
            extractEvery m = map snd . filter (\x -> (fst x `mod` m) == 0) . zip ns
            in map (`extractEvery` xs) ns

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | b > a && b > c = b: p
    | otherwise = p
    where
        p = localMaxima(b:c:xs)
localMaxima _ = []

-- Exercise 3

histogram:: [Integer] -> String
histogram xs =
        let count = map (\n -> length $ filter (== n) xs) [0..9]
            maxi = maximum count
            histo m (base, c) = show base ++ "=" ++
                replicate c '*' ++
                replicate (m - c) ' '
        in
           unlines . reverse . transpose . map (histo maxi) $ zip [0..9] count
