module HW04 where

import BST
import Data.Char
import Data.List

-- == Excercise 1 ==
-- This is the only possible definition for this type signature.
-- Since we don't know anything about a and b the only thing we can do is
-- select one and since we need to return something of type b then we need to
-- always select our second argument.
ex1 :: a -> b -> b
ex1 _ y = y

-- == Excercise 2 ==
-- The only thing a function with this signature can do is return either
-- the first or second argument.
ex2 :: a -> a -> a
ex2 x _ = x

-- == Excercise 3 ==
-- The Int in this excercise is effectively useless. Because a is a fully polymorphic type
-- we don't have any meaningful way to combine Int and a such that we get another a.
-- As such we always want to select our second parameter
ex3 :: Int -> a -> a
ex3 _ x = x

-- == Excercise 4 ==
-- We can use the Bool here to select one of the parameters or we could
-- completely ignore it and just arbitrary select one of our other parameters
--
-- We could create infinitely many distinct functions by combining the bool parameter with itself
-- using any boolean operation (&&, ||, etc...) to create as many definitions inhabiting this type
-- as we please. One could argue that all of these boolean permutations are reducable to a smaller
-- finite set of functions but without boolean reduction we can create as many as we want.
ex4 :: Bool -> a -> a -> a
ex4 b x y = if b then x else y

-- == Excercise 5 ==
-- We can either select our parameter or ignore it and just arbitrary return True or False.
--
-- Thanks to Boolean operations (||, &&) there are infinitely many definitions inhabiting this type.
-- Again we could argue that boolean reduction makes all of those definitions equilvalent but that
-- doesn't mean I can't write infinitely symbols that inhabit this type regardless of if they
-- resolve to the same result.
ex5 :: Bool -> Bool
ex5 x = x

-- == Excercise 6 ==
-- Here we have a function that asks for a function taking a and returning a but we have no
-- a's to operate on. We are asked to return an a but only have a mapping from a to a without an a to start from.
ex6 :: (a -> a) -> a
ex6 _ = error "Impossible"

-- == Excercise 7 ==
-- This function will always be defined by either returning the original parameter or by applying f an
-- arbitrary number of times and returning the result.
--
-- As a result there are infinitely many definitions inhabiting this type.
ex7 :: (a -> a) -> a -> a
ex7 f x = f $ f x

-- == Excercise 8 ==
-- We can return the entire list or a sublist or any permutation using the elements of the list.
--
-- As a result there are infinitely many definitions inhabiting this type.
ex8 :: [a] -> [a]
ex8 (_:xs) = xs
ex8 [] = []

-- == Excercise 9 ==
-- In addition to just returning everything we can in ex8 and ignoring f we can also apply
-- f to any part of the list while we're in the process of forming our return list.
--
-- This is also the type signature of map.
--
-- There are infinitely many definitions inhabiting this type.
ex9 :: (a -> b) -> [a] -> [b]
ex9 f (x:xs) = f x : ex9 f xs
ex9 _ [] = []

-- == Excercise 10 ==
-- There is no way to make this function total. If we have a Maybe constructed with Nothing then there's
-- no way we can construct an a.
ex10 :: Maybe a -> a
ex10 (Just m) = m
ex10 Nothing = error "Why!"

-- == Excercise 11 ==
-- Since we're returning a Maybe our result is going to be either (Just a) or Nothing.
-- We could also recursively wrap and unwrap our created Just using fromJust to allow us
-- infinitely many definitions inhabiting this type.
ex11 :: a -> Maybe a
ex11 x = Just x

-- == Excercise 12 ==
-- As with Excercise 11 we could abuse fromJust to allow us infinitely many definitions inhabiting this type.
-- In general we will take a Maybe but we could always ignore it and return Nothing or we could
-- create a Just. If we do return a Just it will always contain the a we passed in since there's no other
-- way to get an a.
ex12 :: Maybe a -> Maybe a
ex12 (Just x) = Just x
ex12 Nothing = Nothing

-- == Excercise 13 ==
-- Write an insertion method for a binary search tree.
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST comp x (Node left y right)
  | c == LT || c == EQ = (Node (insertBST comp x left) y right)
  | otherwise = (Node left y (insertBST comp x right))
  where c = comp x y

-- == Excercise 14 ==
-- Check to see if a list of strings contains only capitlized words
allCaps :: [String] -> Bool
allCaps = all isCapitalised
  where isCapitalised :: String -> Bool
        isCapitalised (x:_) = isUpper x
        isCapitalised [] = False

-- == Excercise 15 ==
-- Drop the trailing whitespace from a string
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = foldr accumulateNonTrailingSpaces ""
  where accumulateNonTrailingSpaces :: Char -> String -> String
        accumulateNonTrailingSpaces c [] | isSpace c = []
                              | otherwise = [c]
        accumulateNonTrailingSpaces c acc = c : acc

-- == Excercise 16 ==
-- Get the first letter (if it exists) of a list of strings
firstLetters :: [String] -> [Char]
firstLetters = foldr accumulateFirstLetters []
  where accumulateFirstLetters :: String -> [Char] -> [Char]
        accumulateFirstLetters (c:_) acc = c:acc
        accumulateFirstLetters [] acc = acc

-- == Excercise 17 ==
-- Render a proper bracketed list given a list of strings
-- Challenge: Lets not use Show
asList :: [String] -> String
asList xs = '[' : intercalate "," xs ++ "]"
