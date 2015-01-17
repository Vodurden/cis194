{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Prelude hiding (foldl, all)

import Data.Aeson
import Data.Monoid
import Data.Foldable
import Data.Either
import Data.List (null)
import Data.Maybe
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- == Excercise 1 ==
-- Write a function that takes JSON values of "Y" or "N" and converts them to True/False values respectively.
ynToBool :: Value -> Value
ynToBool (Object m) = Object (fmap ynToBool m)
ynToBool (Array v) = Array (fmap ynToBool v)
ynToBool (String text)
  | text == "Y" = Bool True
  | text == "N" = Bool False
  | otherwise = String text
ynToBool v = v

ynToBoolWorks :: Bool
ynToBoolWorks =
  let checkValue :: Value -> Bool
      checkValue (Object m) = foldl (\acc v -> acc && checkValue v) True m
      checkValue (Array m) = foldl (\acc v -> acc && checkValue v) True m
      checkValue (Bool _) = True
      checkValue (String _) = False -- If we have any string fields then we fail.
      checkValue _ = True -- Other fields are fine and should be left alone.

      testData = fmap B.pack [
        "[\"Y\"]",
        "{ \"key\": \"Y\" }"]

      parsed = fmap eitherDecode' testData :: [Either String Value]
      applied = fmap (fmap ynToBool) parsed
      successes = rights applied
      failures = lefts applied
  in null failures && all checkValue successes

-- == Excercise 2 ==
-- Write a function that takes a ByteString and outputs either an error message or the parsed
-- JSON data that is also processed by ynToBool
parseData :: B.ByteString -> Either String Value
parseData bytes = fmap ynToBool $ eitherDecode' bytes

-- == Excercise 3 ==
-- Write a Market type including fields that interest.
-- Write a function parseMarkets that parses the list of markets in the file.
data Market = Market { marketname :: T.Text
                       , x :: Double
                       , y :: Double
                       , state :: T.Text
                       , cheese :: Bool
                       , prepared :: Bool}
            deriving (Eq, Show, Generic)

instance FromJSON Market

resultToEither :: Result a -> Either String a
resultToEither (Success a) = Right a
resultToEither (Error s) = Left s

-- fromJSON returns a Result type which is effectively equivalent to Either (with better semantics)
-- but this assignment specifically wants an either so we need to monadically combine our
-- either produced by parsing to make sure we capture all parsing errors.
--
-- TODO: Check that I'm not missing some obvious easier way to do this without using >>=
parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bytes = (parseData bytes) >>= (resultToEither . fromJSON)

-- == Excercise 4 ==
-- Write a function loadData that loads the market data.
-- If parsing fails report the error with fail.
loadData :: IO [Market]
loadData = do
  filedata <- B.readFile "markets.json"
  let marketData = parseMarkets $ filedata
  case marketData of
   Left err -> fail err
   Right markets -> return markets

-- == Excercise 5 ==
-- Write OrdList and its Monoid instance
data OrdList a = OrdList { getOrdList :: [a] }
               deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList leftAll@(left:leftRest)) (OrdList rightAll@(right:rightRest))
    | left < right = OrdList (left : (getOrdList ((OrdList leftRest) <> (OrdList rightAll))))
    | otherwise = OrdList (right : (getOrdList ((OrdList leftAll) <> (OrdList rightRest))))
  mappend (OrdList []) right = right
  mappend left (OrdList []) = left


ordListWorks :: Bool
ordListWorks =
  (OrdList [2,4,6] <> OrdList [1,3,5] == OrdList [1,2,3,4,5,6])
  && (OrdList [1,3,5] <> OrdList [2,4,6] == OrdList [1,2,3,4,5,6])
  && (OrdList [1,2,7] <> OrdList [] == OrdList [1,2,7])

-- == Excercise 6 ==
-- Write a searching function that given a Monoid and searcher
-- searches through the list of markets for markets whose names contain
-- the given text. THen use the given Market -> Monoid method to convert
-- the results.
type Searcher m = T.Text -> [Market] -> m

marketNameMatches :: T.Text -> Market -> Bool
marketNameMatches text (Market {marketname = name}) = T.isInfixOf text name

search :: Monoid m => (Market -> m) -> Searcher m
search convert text markets = mconcat $ map convert $ filter (marketNameMatches text) markets

-- == Excercise 7 ==
-- Write a function that returns the first market found by a search (if any).
--
-- Bonus points: Define without function arguments
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 listToMaybe (search (:[]))

-- == Excercise 8 ==
-- Write a function that returns the last market found by a search
lastFound :: Searcher (Maybe Market)
lastFound = compose2 (listToMaybe . reverse) (search (:[]))

-- == Excercise 9 ==
-- Write a function that returns all the markets found by a search
allFound :: Searcher [Market]
allFound = search (:[])

-- == Excercise 10 ==
-- Write a function that returns the number of markets found by a search
numberFound :: Searcher Int
numberFound = compose2 getSum (search (\_ -> Sum 1))

-- == Excercise 11 ==
-- Write a function that returns all the markets found by a search ordered
-- from northernmost to southernmost.
newtype NtoSMarket = NtoSMarket Market
                   deriving (Eq)

getNtoSMarket :: NtoSMarket -> Market
getNtoSMarket (NtoSMarket market) = market

-- We consider negative latitude to be south.
-- As such we want the most positive latitude to come before the least positive
instance Ord NtoSMarket where
  compare (NtoSMarket left) (NtoSMarket right)
    | y left < y right = GT
    | y left == y right = EQ
    | otherwise = LT

orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 ((map getNtoSMarket) . getOrdList) (search (\m -> (OrdList [NtoSMarket m])))

-- == Excercise 12 ==
-- Look at the data. Build a query that tells us something interesting.
--
-- In our case we're looking at cheese producers and how cheese production correlates in
-- the northern and souther hemispheres.
-- UPDATE: It turns out this dataset has no dataset from the southern hemisphere so lets scratch this idea!
cheeseProducers :: Searcher [Market]
cheeseProducers = compose2 (filter (cheese)) (search (:[]))

northernCheeseProducers :: Searcher [Market]
northernCheeseProducers = compose2 (filter (\m -> y m >= 0)) (cheeseProducers)

southernCheeseProducers :: Searcher [Market]
southernCheeseProducers = compose2 (filter (\m -> y m < 0)) (search (:[]))

-- New idea: Since our dataset has no useful data about the southern hemisphere (which makes sense
-- retrospectively since it's an american dataset!). Lets look at all the cheesemakers who also
-- have prepared goods vs all those who don't!
wholesomeCheeseProducers :: Searcher [Market]
wholesomeCheeseProducers = compose2 (filter (not . prepared)) (cheeseProducers)

selloutCheeseProducers :: Searcher [Market]
selloutCheeseProducers  = compose2 (filter (prepared)) (cheeseProducers)

cheeseProducerComparison :: [Market] -> ((String, Int), (String, Int))
cheeseProducerComparison markets = (wholesome, sellout)
  where wholesome = ("Wholesome", length $ wholesomeCheeseProducers "" markets)
        sellout = ("Sellout", length $ selloutCheeseProducers "" markets)

-- From the current dataset we have the following results:
-- λ> fmap cheeseProducerComparison loadData
-- (("Wholesome",515),("Sellout",1742))
--
-- Which shows that there are 515 wholesome cheese makers while a whopping 1742 sellout
-- cheese makers. Long live preparation! (This study may or may not be scientific. (Hint: it's not))
