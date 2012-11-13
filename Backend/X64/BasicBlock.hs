module Backend.X64.BasicBlock (
  BasicBlock(..),
  Id,
  empty,
  addInsn,
  setCtrlInsn
) where

import Backend.X64.Insn

data BasicBlock = BasicBlock {
  insnList :: [Insn],
  ctrlInsn :: Maybe Insn,
  predBlock :: [Id],
  succBlock :: [Id]
}
  deriving (Show, Eq)

type Id = Int

empty = BasicBlock {
  insnList = [],
  ctrlInsn = Nothing,
  predBlock = [],
  succBlock = []
}

addInsn :: Insn -> BasicBlock -> BasicBlock
addInsn insn bb = bb {
  insnList = insn:insnList bb
}

setCtrlInsn :: Insn -> BasicBlock -> BasicBlock
setCtrlInsn insn bb = bb {
  ctrlInsn = Just insn
}
