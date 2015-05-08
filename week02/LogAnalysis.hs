{- OPTIONS_GHC -Wall -}

module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I":t:xs) -> LogMessage Info (read t) (unwords xs)
  ("W":t:xs) -> LogMessage Warning (read t) (unwords xs)
  ("E":c:t:xs) -> LogMessage (Error $ read c) (read t) (unwords xs)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) m = m
insert m Leaf = Node Leaf m Leaf
insert m (Node l lg r)
  | mT < logT = Node (insert m l) lg r
  | otherwise = Node l lg (insert m r)
  where
    getTimeStamp = \x -> case x of (LogMessage _ t _)-> t
                                   _ -> 0
    mT = getTimeStamp m
    logT = getTimeStamp lg

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lg r) = inOrder l ++ [lg] ++ inOrder r

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter rel . inOrder . build
  where rel (LogMessage (Error e) _ _) = e >= 50
        rel _ = False
        getMsg (LogMessage _ _ m) = m
        getMsg _ = ""
