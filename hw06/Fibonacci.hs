{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = let fibs2' a b = a : (fibs2' b $ a + b)
        in fibs2' 0 1

-- Exercise 3

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a b) = a : streamToList b

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a b) = Stream (f a) $ streamMap f b

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a $ streamFromSeed f $ f a

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = streamMap ruler' $ streamFromSeed succ 1

ruler' :: Integer -> Integer
ruler' 0 = 0
ruler' n = last $ takeWhile (\x -> n `mod` 2^x == 0) [0..]

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a b) c = Stream a $ interleaveStreams c b

-- Exercise 6

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate = streamMap negate
  (Stream a as) + (Stream b bs) = Stream (a + b) (as + bs)
  (Stream a as) * (Stream b bs) =
    Stream (a * b) $ (bs * fromInteger a) + (as * (Stream b bs))

instance Fractional (Stream Integer) where
  (Stream a as) / (Stream b bs) =
    Stream (a `div` b) $ (as - bs) / (fromInteger b)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
