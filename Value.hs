module Value (Value (..)) where

import Language.ECMAScript3.Syntax

import Data.Map as Map (Map, insert, lookup, union, toList, empty)

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
    | Function Id [Id] [Statement]
    | Break
    | Empty Value
    | Return Value
    | Global (Map String Value)
    | List [Value] deriving (Eq, Ord)

--
-- Pretty Printer
--

instance Show Value where
  show (List l)     = "[" ++ showLis(List l) ++ "]"
  show (Break)      = "Break"
  show (Bool True)  = "true"
  show (Bool False) = "false"
  show (Int int)    = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name)   = name
  show Nil          = "undefined"


showLis :: Value -> String
showLis (List [])   = ""
showLis (List [l])   = show l
showLis (List (l:ls) ) = show l ++ ", " ++ (showLis (List ls))

-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.

showListContents :: [Value] -> String
showListContents []     = ""
showListContents [a]    = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
