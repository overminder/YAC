module Frontend.ObjModel where

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
listToCell = foldr accu Nil
  where
    accu a d = Pair a d

