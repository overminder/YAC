module Backend.IR.Tree (
  Tree(..),
  fromList,
  toList
) where

import Backend.IR.IROp

data Tree = Leaf IROp
          | Seq Tree Tree
          | Add Tree Tree
          | Move Tree Tree
          | Deref Tree
          | Return Tree
          | Nop
  deriving (Eq)

instance Show Tree where
  show (Leaf op) = show op
  show (Seq t0 t1) = show t0 ++ "\n" ++ show t1
  show (Add t0 t1) = show t0 ++ " + " ++ show t1
  show (Move t0 t1) = show t0 ++ " := " ++ show t1
  show (Deref t) = "*(" ++ show t ++ ")"
  show (Return t) = "[return " ++ show t ++ "]"
  show Nop = "nop"

fromList :: [Tree] -> Tree
fromList = foldr Seq Nop

toList :: Tree -> [Tree]
toList (Seq t0 t1) = (t0:toList t1)
toList Nop = []
toList t@_ = [t]

