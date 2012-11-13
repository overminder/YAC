module Frontend.ObjModel (
  Cell(..),
  isList
) where

import qualified Data.List as List

data Cell = Fixnum Int
          | Symbol String
          | List [Cell]
          | DottedList [Cell] Cell
          | Boolean Bool
  deriving (Eq)

instance Show Cell where
  show (Fixnum i) = show i
  show (Symbol s) = s
  show (List xs) = "(" ++ List.intercalate " " (map show xs) ++ ")"
  show (DottedList xs x)
    = "(" ++ List.intercalate " " (map show xs) ++ " . " ++ show x ++ ")"
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"

isList :: Cell -> Bool
isList (List _) = True
isList _ = False

