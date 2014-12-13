-- 'Homework' for: http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf
module HW01 where

import Data.Char (digitToInt)
import Data.Maybe
import qualified Data.Map.Strict as Map

-- ===================================
-- Ex 1: Credit Card Validation
-- ===================================

-- Gets the last digit of an integer.
lastDigit :: Integer -> Integer
lastDigit x = x - ((dropLastDigit x) * 10)

-- Gets all digits but the first of an integer.
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Converts positive integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = map (fromIntegral . digitToInt) (show x)

applyAlternating :: (a -> b) -> (a -> b) -> [a] -> [b]
applyAlternating =
  let applyFirst f g (x:xs) = (f x) : applySecond f g xs
      applyFirst _ _ [] = []
      applySecond f g (x:xs) = (g x) : applyFirst f g xs
      applySecond _ _ [] = []
  in applyFirst

-- Doubles every other integer starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . applyAlternating (*1) (*2) . reverse

-- Sums all the digits in a list for example
-- [1,2,15,62,5] is equivalent to the sum of [1,2,1,5,6,2,5] or 22
sumDigits :: [Integer] -> Integer
sumDigits = foldl (+) 0 . concat . map toDigits

-- Validates if an integer could be used as a valid credit card number
-- An integer is a valid credit card number if it passes the following test:
--
--     1. Take the credit card number and double the value of every second digit from the right
--     2. Add the DIGITS of all the values (doubled and undoubled).
--     3. Divide the result by 10
--     4. If the remainder is 0 then it's valid. Otherwise it's not.
validate :: Integer -> Bool
validate x =
  let doubleDigitSum = sumDigits $ doubleEveryOther $ toDigits x
      remainder = doubleDigitSum `mod` 10
  in remainder == 0

-- ===================================
-- Ex 2: Tower of Hanoi solver
-- ===================================
-- Goal: move all disks from peg A to peg B
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start end _ = [(start, end)]
hanoi 2 start end storage = [(start, storage), (start, end), (storage, end)]
hanoi n start end storage = (hanoi (n-1) start storage end) ++ [(start, end)] ++ (hanoi (n-1) storage end start)

-- Super Duper Hanoi: Now with r pegs!
-- Since we're doing four why not do more?
-- We basically want to move the top k pegs of the tower to one storage then move the rest to
-- the end and move the original top back.
--
-- Interestingly the solution is only 'presumed optimal' via mechanical search because
-- choosing how to distribute the pegs between the storages is hard. To make sure
-- you have an optimal k you effectively have to try all values between 1 and the number of disks
-- to be sure you've picked a k that results in the lowest number of steps.
--
-- I've looked at some small output and it looks correct but this hasn't been tested. (Update: Some testing has been done)
-- (Idea: Build a simulator taking the output of these functions and checking that each move is valid)
-- (Update: Simulator built. hanoi and superDuperHanoi seem to work well assuming the simulator is valid
--          unfortunately superHanoi did not make the cut and has been removed!)
superDuperHanoi :: Integer -> [Peg] -> [Move]
superDuperHanoi _ [] = [] -- No pegs? No problem!
superDuperHanoi _ [_] = [] -- One peg? Still no problem!
superDuperHanoi 0 _ = [] -- Zero disks? No problem!
superDuperHanoi 1 (start:end:_) = [(start, end)]
superDuperHanoi _ [_, _] = error "More pegs are needed for towers of size > 1"
superDuperHanoi n (start:end:storage:rest) =
  let k
        | (length rest) > 0 = (n `div` 2)
        | otherwise = n - 1 -- Just move all but the last when we have less then four disks
      upperHeight = k
      lowerHeight = n - k
      moveUpperToStorage = superDuperHanoi upperHeight (start:storage:end:rest)
      moveLowerToEnd = superDuperHanoi lowerHeight (start:end:rest) -- Ignore storage since it's used
      moveUpperToEnd = superDuperHanoi upperHeight (storage:end:start:rest)
  in moveUpperToStorage ++ moveLowerToEnd ++ moveUpperToEnd

-- Validates that a hanoi solution doesn't violate any hanoi rules
validateHanoiSolution :: Integer -> [Peg] -> [Move] -> Bool
validateHanoiSolution disks pegs moves =
  -- Initial state represents the size of the disk at the top of each peg.
  -- We treat the smallest disk as size 1 with the largest being of size `disks`
  let initialState = Just (buildInitialPegState disks pegs)
  in isJust $ foldl applyMove initialState moves

applyMove :: Maybe (Map.Map Peg [Integer]) -> Move -> Maybe (Map.Map Peg [Integer])
applyMove Nothing _ = Nothing
applyMove (Just state) (from, to)
  | (Map.notMember from state) || (Map.notMember to state) = Nothing
  | null $ (fromJust $ Map.lookup from state) = Nothing -- If we're moving from empty we're invalid
  | otherwise =
      let fromStack = fromJust $ Map.lookup from state
          toStack = fromJust $ Map.lookup to state

          -- We've already checked that we're not moving from a null space
          -- but we want to check that we're moving to a valid space (bigger or empty)
          movingOntoValidSpace = (null toStack) || (head toStack > head fromStack)

          adjustFrom = Map.adjust (tail) from state
          adjustTo = Map.adjust ((head fromStack):) to adjustFrom
      in if movingOntoValidSpace then Just adjustTo else Nothing

buildInitialPegState :: Integer -> [Peg] -> Map.Map Peg [Integer]
buildInitialPegState _ [] = Map.empty
buildInitialPegState disks (start:others) =
  let startState = (start, [1..disks])
      rest = map (\x -> (x, [])) others
  in Map.fromList $ startState : rest

-- According to the homework page a 4 peg 15 disk problem can be solved in
-- 129 moves. The best I've got so far is 305 moves with the superDuperHanoi algorithm.
-- Further study is needed (possibly try other values for k)
runHanoi :: Integer -> [(String, Int, Bool)]
runHanoi n =
  let pegs = ["a", "b", "c", "d"] :: [Peg]

      run :: String -> [Peg] -> [Move] -> (String, Int, Bool)
      run name p moves = (name, length moves, validateHanoiSolution n p moves)

      hanoiResults = run "Hanoi" ["a", "b", "c"] (hanoi n "a" "b" "c")
      superDuperHanoiResults = run "Super Duper Hanoi" pegs (superDuperHanoi n pegs)
  in [hanoiResults, superDuperHanoiResults]
