module Frontend.ObjModel (
  Cell(..),
  listToCell,
  dottedListToCell,
  isProperList,
  pairToList
) where

data Cell = Fixnum Int
          | Symbol String
          | Pair Cell Cell
          | Boolean Bool
          | Nil
  deriving (Eq)

instance Show Cell where
  show (Fixnum i) = show i
  show (Symbol s) = s
  show p@(Pair _ _) = "(" ++ showListHead p ++ ")"
    where
      showListHead (Pair a d) = show a ++ showListTail d
      showListTail d@(Pair _ _) = " " ++ showListHead d
      showListTail Nil = ""
      showListTail d = " . " ++ show d
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show Nil = "()"

listToCell :: [Cell] -> Cell
listToCell = (flip dottedListToCell) Nil

dottedListToCell :: [Cell] -> Cell -> Cell
dottedListToCell = flip (foldr Pair)

isProperList :: Cell -> Bool
isProperList (Pair _ cdr) = isProperList cdr
isProperList Nil = True
isProperList _ = False

pairToList :: Cell -> ([Cell], Cell)
pairToList (Pair car cdr) = (car:cdrs, left)
  where
    (cdrs, left) = pairToList cdr
pairToList Nil = ([], Nil)
pairToList c = ([], c)

