{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage message =
  let messageParts = words message;
      messageType = head messageParts
  in case messageType of
    "I" -> parseInfo messageParts
    "W" -> parseWarning messageParts
    "E" -> parseError messageParts
    _   -> Unknown message

parseInfo :: [String] -> LogMessage
parseInfo (_:timestamp:message) = LogMessage Info (read timestamp) (unwords message)
parseInfo parts = Unknown (unwords parts)

parseWarning :: [String] -> LogMessage
parseWarning (_:timestamp:message) = LogMessage Warning (read timestamp) (unwords message)
parseWarning parts = Unknown (unwords parts)

parseError :: [String] -> LogMessage
parseError (_:severity:timestamp:message) = LogMessage (Error $ read severity) (read timestamp) (unwords message)
parseError parts = Unknown (unwords parts)

parse :: String -> [LogMessage]
parse =  map parseMessage . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg tree =
  let (LogMessage _ ts _) = msg;
      (Node left otherMessage right) = tree;
      (LogMessage _ otherTs _) = otherMessage
  in if ts < otherTs then (Node (insert msg left) otherMessage right)
     else (Node left otherMessage (insert msg right))

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ msg) -> msg) . filter isRelevant . inOrder . build
  where isRelevant :: LogMessage -> Bool
        isRelevant (LogMessage (Error severity) _ _) = severity >= 50
        isRelevant _ = False

