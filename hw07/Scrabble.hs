{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char (toUpper)
import Data.Monoid
import qualified Data.Set as Set

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score x) = x

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

scrabbleTiles :: Set.Set Char
scrabbleTiles = Set.fromList ['A'..'Z']

score :: Char -> Score
score c
  | c' `Set.notMember` scrabbleTiles = Score 0
  | c' `elem` "QZ"    = Score 10
  | c' `elem` "JX"    = Score 8
  | c' == 'K'         = Score 5
  | c' `elem` "FHVWY" = Score 4
  | c' `elem` "BCMP"  = Score 3
  | c' `elem` "DG"    = Score 2
  | otherwise         = Score 1
  where c' = toUpper c

scoreString :: String -> Score
scoreString = mconcat . map score

