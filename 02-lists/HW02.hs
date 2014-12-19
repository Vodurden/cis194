module HW02 where

import Words
import Data.List
import Data.Char()

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Determines if a word is createable given an unordered hand of letters
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:xs) hand = (x `elem` hand) && formableBy xs (delete x hand)

-- Determines all the scrabble words creatable from a hand of letters
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- Given a template where ? denotes a wildcard and other letters denote a constraint
-- determines if a given word can be fit exactly into the template using the existing letters
-- and the hand.
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate template hand word
  | fitsTemplate && formable = True
  | otherwise = False
  where
    matches = zipWith (\x y -> x == '?' || x == y) template word
    fitsTemplate = (length template == length word) && (all (id) matches)
    formable = formableBy word (hand ++ template)

-- Given a template (see above) and a hand determine all the scrabble words that can be formed
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand =
  filter (wordFitsTemplate template hand) allWords

-- Get the scrabble value of a word
scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . (map scrabbleValue)

-- Filters a list of words to only contain words that have the best scrabble value. If there are a tie multiple words are returned
bestWordsHelper :: (String -> Int) -> [String] -> [String]
bestWordsHelper getValue =
  let accumulateWord :: [String] -> String -> [String]
      accumulateWord [] word = [word]
      accumulateWord ws@(x:xs) word
        | xValue == wordValue = word:ws
        | otherwise = x:xs
        where xValue = getValue x
              wordValue = getValue word
  in foldl' accumulateWord []

bestWords :: [String] -> [String]
bestWords = bestWordsHelper scrabbleValueWord

-- Determines the letter multiplier for all characters in a STemplate
letterMultipliers :: STemplate -> [Int]
letterMultipliers = map getMultiplier
                   where getMultiplier :: Char -> Int
                         getMultiplier 'T' = 3
                         getMultiplier 'D' = 2
                         getMultiplier _ = 1

-- Determines the global word multiplier for a STemplate character
wordMultiplier :: STemplate -> Int
wordMultiplier = product . map getMultiplier
                 where getMultiplier :: Char -> Int
                       getMultiplier '3' = 3
                       getMultiplier '2' = 2
                       getMultiplier _ = 1

-- Determines the value of a word when applied to a given STemplate.
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate template word =
  wordValueWithLetterMultiplier * globalMultiplier
  where
    values = map scrabbleValue word
    letterValues = letterMultipliers template
    wordValueWithLetterMultiplier = sum $ zipWith (*) letterValues values
    globalMultiplier = wordMultiplier template


-- Fits templates of the form ??x?? but doesn't require trailing ?'s are filled.
extendedWordFitsTemplate :: Template -> Hand -> String -> Bool
extendedWordFitsTemplate template hand word
  | fitsInsideTemplate && formable && matches = True
  | otherwise = False
  where
    fitsInsideTemplate = (length word <= length template)
    special = ['?', '2', '3', 'D', 'T']
    matches = foldl' (\b (x, y) -> b && (x `elem` special || x == y)) True (zip template word)
    formable = formableBy word (hand ++ template)

-- Determines the best words to play given a particular template and hand
whatToPlay :: STemplate -> Hand -> [String]
whatToPlay template hand =
  let candidateWords = filter (extendedWordFitsTemplate template hand) allWords
      best = bestWordsHelper (scrabbleValueTemplate template) candidateWords
  in best
