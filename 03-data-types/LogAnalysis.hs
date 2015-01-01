{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.Char (isDigit, toLower)
import Data.List (sortBy, isInfixOf)

import Log

allDigits :: String -> Bool
allDigits = all (isDigit)

(!!?) :: [a] -> Int -> Maybe a
(!!?) list index | length list > index = Just (list !! index)
                 | otherwise = Nothing

stringToInt :: String -> Maybe Int
stringToInt s | allDigits s = Just (read s)
              | otherwise = Nothing

-- == Excercise 1 ==
-- Parse individual strings into MaybeLogMessages
--
-- TODO: Think about how this method is structured.
--       It feels awkward and there's probably a better way to compose this
--       instead of multiple do blocks.
parseMessageM :: String -> Maybe LogMessage
parseMessageM s =
    case tokens of
          ("E":sever:timestamp:rest) -> do
            sev <- stringToInt sever
            time <- stringToInt timestamp
            return (LogMessage (Error sev) time (unwords rest))
          ("W":timestamp:rest) -> do
            time <- stringToInt timestamp
            return (LogMessage Warning time (unwords rest))
          ("I":timestamp:rest) -> do
            time <- stringToInt timestamp
            return (LogMessage Info time (unwords rest))
          _ -> Nothing
    where tokens = words s

-- Parse a message of the form <Type> <Time> <Message>.
parseMessage :: String -> MaybeLogMessage
parseMessage s = case mes of
                  (Just m) -> ValidLM m
                  Nothing -> InvalidLM s
  where mes = parseMessageM s

-- == Excercise 2 ==
-- Filter MaybeLogMessages into only valid LogMessages
isValidLogMessage :: MaybeLogMessage -> Bool
isValidLogMessage (ValidLM _) = True
isValidLogMessage (InvalidLM _) = False

fromMaybeLogMessage :: MaybeLogMessage -> LogMessage
fromMaybeLogMessage (ValidLM m) = m
fromMaybeLogMessage (InvalidLM _) = error "Cannot get LogMessage from invalid log message"

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly = map fromMaybeLogMessage . filter isValidLogMessage

-- == Exercise 3 ==
-- Use things defined earlier to parse a set of strings into Log Messages
parse :: String -> [LogMessage]
parse s = validMessagesOnly $ map parseMessage $ lines s


-- == Excercise 4 ==
-- Compare our parsed log messages - only by timestamp not error type
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ leftStamp _) (LogMessage _ rightStamp _) = compare leftStamp rightStamp

-- == Excercise 5 ==
-- Sort our log messages based on the above comparison
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

-- == Excercise 6 ==
-- Use the above to figure out what errors are relevant
-- An error is relevant if it has a severity of at least 50
severity :: LogMessage -> Int
severity (LogMessage (Error sev) _ _) = sev
severity _ = 0

message :: LogMessage -> String
message (LogMessage _ _ m) = m

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

isSevere :: LogMessage -> Bool
isSevere m = isError m && severity m > 50

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . sortMessages . filter isSevere

-- == Excercise 7 ==
-- Find messages that contain a (case-insensitive) string
logMessageContains :: String -> LogMessage -> Bool
logMessageContains search m = isInfixOf (map toLower search) (map toLower $ message m)

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout search messages = filter (logMessageContains search) messages

-- == Excercise 8 ==
-- Combined output with high-severity messages and all messages containing a keyword

-- Extra for using the following function in whatWentWrongEnhanced
(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced search messages = map message
                                        $ sortMessages
                                        $ filter (isSevere ||| logMessageContains search) messages
