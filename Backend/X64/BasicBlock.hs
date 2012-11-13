module Backend.X64.BasicBlock (
  BasicBlock(..),
  Id,
  empty,
  hasNoInsn,
  addInsn,
  addLabel,
  setCtrlInsn,
  getFullInsnList,
  setFullInsnList,
  getFirstInsn,
  toTrace
) where

import qualified Data.List as List

data BasicBlock a = BasicBlock {
  bId :: Id,
  insnList :: [a],
  ctrlInsn :: Maybe a,
  labels :: [a]
}
  deriving (Show, Eq)

instance Functor BasicBlock where
  fmap f bb = bb {
    insnList = fmap f $ insnList bb,
    ctrlInsn = fmap f $ ctrlInsn bb,
    labels = fmap f $ labels bb
  }

type Id = Int

empty = BasicBlock {
  bId = -1,
  insnList = [],
  ctrlInsn = Nothing,
  labels = []
}

hasNoInsn :: BasicBlock a -> Bool
hasNoInsn BasicBlock{insnList = []} = True
hasNoInsn _ = False

addInsn :: a -> BasicBlock a -> BasicBlock a
addInsn insn bb = bb {
  insnList = insnList bb ++ [insn]
}

addLabel :: a -> BasicBlock a -> BasicBlock a
addLabel label bb = bb {
  labels = label:labels bb
}

setCtrlInsn :: a -> BasicBlock a -> BasicBlock a
setCtrlInsn insn bb = bb {
  ctrlInsn = Just insn
}

getFullInsnList :: BasicBlock a -> [a]
getFullInsnList bb = case ctrlInsn bb of
  (Just insn) -> insnList bb ++ [insn]
  Nothing -> insnList bb

setFullInsnList :: [a] -> BasicBlock a -> BasicBlock a
setFullInsnList xs bb = case ctrlInsn bb of
  (Just _) -> bb {
    insnList = List.init xs,
    ctrlInsn = Just $ List.last xs
  }
  Nothing -> bb {
    insnList = xs
  }

getFirstInsn :: BasicBlock a -> a
getFirstInsn bb = case bb of
  BasicBlock{insnList=x:xs} -> x
  _ -> error "BasicBlock.getFirstInsn: this block is empty!"

toTrace :: BasicBlock a -> [a]
toTrace bb = labels bb ++ getFullInsnList bb

