module Backend.X64.Munch where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Backend.IR.Operand as IROp
import qualified Backend.IR.Tree as IRTree
import Backend.X64.Insn

data MunchState = MunchState {
  nextId :: Int,
  insnList :: [Insn]
}

emptyMunchState :: MunchState
emptyMunchState = MunchState {
  nextId = 0,
  insnList = []
}

type Muncher = State MunchState

execMunch :: IRTree.Tree -> MunchState
execMunch t = execState (munchTree t) emptyMunchState

munch :: IRTree.Tree -> [Insn]
munch = insnList . execMunch

newPseudoReg :: Muncher Operand
newPseudoReg = liftM (IROperand . IROp.RegOperand . IROp.PseudoReg) genNextId
  where
    genNextId :: Muncher Int
    genNextId = do
      s <- get
      let i = nextId s
      put s{nextId = i + 1}
      return i

getInsnList :: Muncher [Insn]
getInsnList = liftM insnList get

putInsnList :: [Insn] -> Muncher ()
putInsnList is = do
  s <- get
  put s{insnList=is}

emitInsn :: Insn -> Muncher ()
emitInsn i = do
  is <- getInsnList
  putInsnList $ is ++ [i]

-- Here comes the pattern matcher

munchTree :: IRTree.Tree -> Muncher (Maybe Operand)
munchTree (IRTree.Add t0 t1) = do
  (Just rand0) <- munchTree t0
  (Just rand1) <- munchTree t1
  munchAdd rand0 rand1
  where
    munchAdd (IROperand (IROp.ImmOperand i0))
             (IROperand (IROp.ImmOperand i1)) = do
      return $ Just $ IROperand $ IROp.ImmOperand $ i0 + i1

    munchAdd (IROperand (IROp.RegOperand r0))
             (IROperand (IROp.ImmOperand i0)) = do
      tempReg <- newPseudoReg
      emitInsn (Lea tempReg (X64Operand (Address r0 Nothing Scale1 i0)))
      return $ Just tempReg

    munchAdd i0@(IROperand (IROp.ImmOperand _))
             r0@(IROperand (IROp.RegOperand _)) =
      munchAdd r0 i0

    munchAdd (IROperand (IROp.RegOperand r0))
             (IROperand (IROp.RegOperand r1)) = do
      tempReg <- newPseudoReg
      emitInsn (Lea tempReg (X64Operand (Address r0 (Just r1) Scale1 0)))
      return $ Just tempReg

-- XXX: manual munch will produce good code at the expense of duplicated
-- code and harder maintainence cost... what to?
munchTree (IRTree.Move (IRTree.Leaf r0@(IROp.RegOperand _))
                       (IRTree.Add t1 t2)) = do
  (Just rand1) <- munchTree t1
  (Just rand2) <- munchTree t2
  munchMoveAdd rand1 rand2
  return Nothing
  where
    munchMoveAdd (IROperand (IROp.RegOperand r1))
                 (IROperand (IROp.RegOperand r2)) = do
      emitInsn (Lea (IROperand r0)
                    (X64Operand (Address r1 (Just r2) Scale1 0)))

    munchMoveAdd (IROperand (IROp.RegOperand r1))
                 (IROperand (IROp.ImmOperand i2)) = do
      emitInsn (Lea (IROperand r0)
                    (X64Operand (Address r1 Nothing Scale1 i2)))

    munchMoveAdd i1@(IROperand (IROp.ImmOperand _))
                 r2@(IROperand (IROp.RegOperand _)) = do
      munchMoveAdd r2 i1

    munchMoveAdd (IROperand (IROp.ImmOperand i1))
                 (IROperand (IROp.ImmOperand i2)) = do
      emitInsn (Mov (IROperand r0) (IROperand (IROp.ImmOperand $ i1 + i2)))

munchTree (IRTree.Move (IRTree.Leaf r0@(IROp.RegOperand _)) t1) = do
  (Just rand1) <- munchTree t1
  munchMove rand1
  return Nothing
  where
    munchMove i0@(IROperand (IROp.ImmOperand _)) = do
      emitInsn (Mov (IROperand r0) i0)
    munchMove r1@(IROperand (IROp.RegOperand _)) = do
      emitInsn (Mov (IROperand r0) r1)

munchTree (IRTree.Move (IRTree.Deref t0) t1) = do
  (Just rand0) <- munchTree t0
  (Just rand1) <- munchTree t1
  munchMove rand0 rand1
  return Nothing
  where
    munchMove (IROperand (IROp.RegOperand r0))
              r1@(IROperand (IROp.RegOperand _)) = do
      emitInsn (Mov (X64Operand (Address r0 Nothing Scale1 0)) r1)
    munchMove (IROperand (IROp.RegOperand r0))
              i0@(IROperand (IROp.ImmOperand ival)) = do
      if isInt32 ival
        then do
          emitInsn (Mov (X64Operand (Address r0 Nothing Scale1 0)) i0)
        else do
          tempReg <- newPseudoReg
          emitInsn (Mov tempReg i0)
          emitInsn (Mov (X64Operand (Address r0 Nothing Scale1 0)) tempReg)

munchTree (IRTree.Deref t) = do
  (Just rand) <- munchTree t
  munchDeref rand
  where
    munchDeref (IROperand (IROp.RegOperand r)) = do
      tempReg <- newPseudoReg
      emitInsn (Mov tempReg (X64Operand (Address r Nothing Scale1 0)))
      return (Just tempReg)

munchTree (IRTree.Seq t0 t1) = do
  munchTree t0
  munchTree t1
  return Nothing

munchTree (IRTree.Leaf op) = do
  return $ Just $ IROperand op

-- Helpers
isInt32 :: Int -> Bool
isInt32 i = floor (- 2 ** 31) <= i && i <= floor (2 ** 31 - 1)

