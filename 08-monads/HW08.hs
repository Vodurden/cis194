module HW08 where

import Control.Monad
import Control.Monad.Random

import Data.List
import Data.Maybe
import Data.Monoid

import Text.Read

-- == Excercise 1 ==
-- Write a function that detects whether or not a string has a certain format as follows:
--
--     1. The string starts with a digit
--     2. Given a digit with a value of n. The string next contains n a's
--     3. After the n as, either the string ends or the sequence repeats starting
--        with another digit (can be the same digit)
--
-- TODO: Think about implementing this without do notation
--       Think about implementing this with applicative instead of monad.
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
  where go :: String -> Maybe String
        go [] = Just ""
        go (first:rest) = do prefix <- readMaybe [first] :: Maybe Int
                             let letters = replicate prefix 'a'
                             newRest <- (stripPrefix letters) rest
                             sPrefix <- return $ show prefix
                             after <- go newRest
                             return $ sPrefix ++ letters ++ after

-- == Excercise 2 ==
-- Use a list comprehension to produce the list of all numbers between 1 and 100 (inclusive)
-- that are divisible by 5 but not by 7.
specialNumbers :: [Int]
specialNumbers = [ n | n <- [1..100], acceptable n ]
                 where acceptable n = n `mod` 5 == 0 && n `mod` 7 /= 0

-- == Excercise 3 ==
-- Write an action that simulates rolling a fair, 6-sided die.
type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
                  deriving (Show)

type DieRoll = Int

type StdRand = Rand StdGen

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1,6)

-- == Excercise 4 ==
-- Write a function that takes the attacker's dice rolls and the defenders
-- dice rolls and computes the change in the number of armies resulting from the rolls
instance Monoid ArmyCounts where
  mempty = ArmyCounts { attackers = 0, defenders = 0 }
  mappend left right = ArmyCounts { attackers = attackers left + attackers right,
                                   defenders = defenders left + defenders right }

zipWithDefault :: (a -> a -> c) -> a -> [a] -> [a] -> [c]
zipWithDefault f d (x:xs) (y:ys) = f x y : zipWithDefault f d xs ys
zipWithDefault f d [] (y:ys) = f d y : zipWithDefault f d [] ys
zipWithDefault f d (x:xs) [] = f x d : zipWithDefault f d xs []
zipWithDefault _ _ [] [] = []

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults attack defense = mconcat $ zipWith combat (reverse $ sort attack) (reverse $ sort defense)
  where combat :: DieRoll -> DieRoll -> ArmyCounts
        combat atk def | atk > def = ArmyCounts {attackers = 0, defenders = -1}
                       | otherwise = ArmyCounts {attackers = -1, defenders = 0}

-- == Excercise 5 ==
-- Write a function which simulates a single battle between two opposing armies
battle :: ArmyCounts -> StdRand ArmyCounts
battle army@(ArmyCounts {attackers = atk, defenders = def}) =
  do attack <- replicateM (min 3 atk) (dieRoll)
     defense <- replicateM (min 2 def) (dieRoll)
     return (army <> battleResults attack defense)

-- == Excercise 6 ==
-- Write a function which simulates an entiree invasion attempt
-- that is: No defenders or fewer then two attackers.
--
-- TODO: Revisit this and think about applicative.
invade :: ArmyCounts -> StdRand ArmyCounts
invade army@(ArmyCounts {attackers = atk, defenders = def})
  | def == 0 || atk < 2 = return army
  | otherwise = do results <- battle army
                   invade results

-- == Excercise 7 ==
-- Write a function which runs invade 1000 times and returns a
-- Double between 0 and 1 representing the simulated probability
-- that the attacking army will completely destroy the defending army.
(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b

successProb :: ArmyCounts -> StdRand Double
successProb army =
  do battles <- replicateM 1000 (invade army)
     successes <- return $ length $ filter isSuccess battles
     return $ successes // 1000
  where isSuccess (ArmyCounts {defenders = 0}) = True
        isSuccess _ = False
