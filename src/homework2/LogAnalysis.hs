{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage st = case str of
  "I" : time : string -> LogMessage Info (read time) (unwords string)
  "W" : time : string -> LogMessage Warning (read time) (unwords string)
  "E" : level : time : string -> LogMessage (Error (read level)) (read time) (unwords string)
  _ -> Unknown st
  where str = words st

parse :: String -> [LogMessage]
parse = map parseMessage . lines 

insert :: MessageTree -> LogMessage -> MessageTree
insert tree message = case message of
  LogMessage _ time _ -> case tree of
    Node left me@(LogMessage _ ti _) right ->
      if (time <= ti) then
        Node (insert left message) me right
      else 
        Node left me (insert right message)
    Node _ (Unknown _) _ -> error "MessageTree is not well defined"
    Leaf -> 
      Node Leaf message Leaf
  Unknown _ -> tree

build :: [LogMessage] -> MessageTree
build = foldl insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
  Node left message right -> (inOrder left) ++ [message] ++ (inOrder right)
  Leaf -> []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = case sorted of
  (LogMessage (Error severity) _ str) : xs -> 
    if (severity > 50) then 
      [str] ++ (whatWentWrong xs)
    else (whatWentWrong xs)
  (_ : xs) -> whatWentWrong xs
  [] -> []
  where sorted = inOrder (build messages)
