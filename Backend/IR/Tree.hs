module Backend.IR.Tree (
  Tree(..),
  CallType(..),
  Cond(..),
  reverseCond,
  fromList,
  toList
) where

import qualified Data.List as List

import Backend.IR.IROp

data CallType = NormalCall | TailCall
  deriving (Show, Eq)

data Tree = Leaf IROp
          | Seq Tree Tree
          | Add Tree Tree
          | Sub Tree Tree
          | ShiftArithLeft Tree Tree
          | Move Tree Tree
          | Deref Tree
          | If Tree Tree Tree
          | Compare Tree Tree Cond
          | Call Tree [Tree] CallType -- func arg tailp
          | Return Tree
          | Nop
  deriving (Show, Eq)

data Cond = Ge | Gt | Le | Lt | Eq | Ne
  deriving (Show, Eq)

reverseCond :: Cond -> Cond
reverseCond c = case c of
  Ge -> Lt
  Gt -> Le
  Lt -> Ge
  Le -> Gt
  Eq -> Ne
  Ne -> Eq

--instance Show Tree where
--  show (Leaf op) = show op
--  show (Seq t0 t1) = show t0 ++ case t1 of
--    Nop -> []
--    _ -> "\n" ++ show t1
--  show (Add t0 t1) = show t0 ++ " + " ++ show t1
--  show (Sub t0 t1) = show t0 ++ " - (" ++ show t1 ++ ")"
--  show (Move t0 t1) = show t0 ++ " := " ++ show t1
--  show (Deref t) = "*(" ++ show t ++ ")"
--  show (If t0 t1 t2) = "[if "  ++ show t0 ++ " then " ++
--    show t1 ++ " else " ++ show t2 ++ "]"
--  show (Call t ts tailp)
--    = show t ++ "." ++ show tailp ++ "(" ++
--      List.intercalate "," (map show ts) ++ ")"
--  show (Return t) = "[return " ++ show t ++ "]"
--  show Nop = error "show Nop: empty program body?"
--  _ = error "Tree.show"

fromList :: [Tree] -> Tree
fromList = foldr Seq Nop

toList :: Tree -> [Tree]
toList (Seq t0 t1) = (t0:toList t1)
toList Nop = []
toList t@_ = [t]

