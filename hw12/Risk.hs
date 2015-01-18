{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (guard, replicateM)
import Control.Monad.Random
import Data.List (sortBy, zip)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield atk def) = do
  attackerRolls <- replicateM attacking die
  defenderRolls <- replicateM defending die
  return $ processRound bf attackerRolls defenderRolls
  where attacking = min 3 (atk - 1)
        defending = min 2 def

processRound :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
processRound (Battlefield atk def) aRolls dRolls =
  Battlefield (atk - atkDeaths) (def - defDeaths)
  where
    sortDesc = sortBy (flip compare)
    matchedRolls = zip (sortDesc aRolls) (sortDesc dRolls)
    (atkDeaths, defDeaths) =
      foldl (\(a1, d1) (a2, d2) -> (a1 + a2, d1 + d2)) (0, 0) $
      map (\(a, d) -> if a > d then (0, 1) else (1, 0)) matchedRolls

-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield =
  do results <- invadeHelper battlefield
     return $ last results

invadeHelper :: Battlefield -> Rand StdGen [Battlefield]
invadeHelper battlefield =
  do result <- battle battlefield
     if (gameOver result)
       then return [result]
       else do results <- invadeHelper result
               return (result:results)
  where gameOver (Battlefield atk def) = atk < 2 || def <= 0

-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield =
  do results <- replicateM 1000 (invade battlefield)
     return (fromIntegral (numSuccess results) / fromIntegral 1000)
  where success (Battlefield atk def) = atk > def
        numSuccess = length . filter id . map success

-- in GHCI:
-- do p <- evalRandIO (successProb (Battlefield 90 120)); putStrLn $ show p

