module Backend.X64.BasicBlock (
  BasicBlock(..),
  Id,
  empty,
  hasNoInsn,
  addInsn,
  addLabel,
  setCtrlInsn
) where

import Backend.X64.Insn

data BasicBlock = BasicBlock {
  bId :: Id,
  insnList :: [Insn],
  ctrlInsn :: Maybe Insn,
  labels :: [Label]
}
  deriving (Show, Eq)

type Id = Int

empty = BasicBlock {
  bId = -1,
  insnList = [],
  ctrlInsn = Nothing,
  labels = []
}

hasNoInsn :: BasicBlock -> Bool
hasNoInsn BasicBlock{insnList = []} = True
hasNoInsn _ = False

addInsn :: Insn -> BasicBlock -> BasicBlock
addInsn insn bb = bb {
  insnList = insnList bb ++ [insn]
}

addLabel :: Label -> BasicBlock -> BasicBlock
addLabel label bb = bb {
  labels = label:labels bb
}

setCtrlInsn :: Insn -> BasicBlock -> BasicBlock
setCtrlInsn insn bb = bb {
  ctrlInsn = Just insn
}

