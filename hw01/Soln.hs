{-# OPTIONS_GHC -Wall #-}

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = lastDigit : toDigitsRev leadingDigits
  where lastDigit     = n `mod` 10
        leadingDigits = n `div` 10

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft [] = []
doubleFromLeft [n] = [n]
doubleFromLeft (a:b:cs) = a : (2*b) : doubleFromLeft cs

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- Exercise 4

validate :: Integer -> Bool
validate cardNumber = digitSum `mod` 10 == 0
  where someDigitsDoubled = doubleEveryOther $ toDigits cardNumber
        digitSum = sumDigits someDigitsDoubled

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a,b)]
hanoi n src dst tmp = srcToTmp ++ [(src, dst)] ++ tmpToDst
  where srcToTmp = hanoi (n-1) src tmp dst
        tmpToDst = hanoi (n-1) tmp dst src

