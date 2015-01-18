{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List (group,sort,transpose,)

-- Exercise 1

skips :: [a] -> [[a]]
skips xs = map (flip dropUntilEmpty xs) [0..(length xs - 1)]
  where dropUntilEmpty :: Int -> [a] -> [a]
        dropUntilEmpty n ys
          | null oth  = []
          | otherwise = head oth : dropUntilEmpty n (drop 1 oth)
          where oth = drop n ys

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, x, _) -> x) $ filter hasMaxima triples
  where triples = zip3 xs (drop 1 xs) (drop 2 xs)
        hasMaxima (a, b, c) = a < b && b > c

-- Exercise 3

histogram :: [Integer] -> String
histogram xs = (++ "==========\n0123456789\n") $ unlines $ transpose stars
  where counts = map (pred . length) $ group $ sort $ [0..9] ++ xs
        maxC = maximum counts
        stars = map (\n -> replicate (maxC - n) ' ' ++ replicate n '*') counts

