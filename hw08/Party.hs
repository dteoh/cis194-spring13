{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Data.List (sort)
import Data.Monoid
import Data.Tree
import Employee


-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL list fun) = GL (emp : list) (empFun emp + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 < f2   = gl2
  | otherwise = gl1

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tree = f (rootLabel tree) (map (treeFold f) $ subForest tree)

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss = newInvite . foldl combineLists (mempty, mempty)
  where combineLists :: (GuestList, GuestList)
                     -> (GuestList, GuestList)
                     -> (GuestList, GuestList)
        combineLists (gl1, gl2) (gl1', gl2') = (gl1 <> gl1', gl2 <> gl2')
        newInvite (withBoss, noBoss) = (glCons boss noBoss, withBoss)

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5

main :: IO ()
main = readFile "company.txt" >>= processFile
  where
    processFile str =
      let hierarchy = read str :: Tree Employee
          (GL guests totalFun) = maxFun hierarchy
          employeeNames = sort $ map empName guests
          outputLines = ("Total fun: " ++ show totalFun) : employeeNames
      in putStrLn $ unlines outputLines

