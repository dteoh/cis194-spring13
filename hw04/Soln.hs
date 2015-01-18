{-# OPTIONS_GHC -Wall #-}

import Data.List ((\\))

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = undefined

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    treeHeight :: Tree a -> Integer
    treeHeight Leaf = -1
    treeHeight (Node h _ _ _) = h

    insert :: a -> Tree a -> Tree a
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node _ l y r)
      | hl <= hr  = let newL = insert x l
                    in Node (succ $ max hr $ treeHeight newL) newL y r
      | otherwise = let newR = insert x r
                    in Node (succ $ max hl $ treeHeight newR) l y newR
      where hl = treeHeight l
            hr = treeHeight r

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr xorOp False

xorOp :: Bool -> Bool -> Bool
xorOp True False = True
xorOp False True = True
xorOp _ _        = False


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x bs ->  f x : bs) []

-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) $ [1..n] \\ nonPrimes
  where pairs = filter (uncurry (<=)) $ cartProd [1..n] [1..n]
        nonPrimes = map (\(i, j) -> i + j + (2 * i * j)) pairs

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

