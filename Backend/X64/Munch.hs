module Backend.X64.Munch (
  munch
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.Oprnd
import qualified Backend.IR.Tree as T
import Backend.IR.Temp
import Backend.X64.Insn

type Muncher = StateT MunchState TempGen

type MunchState = [Insn]

emptyMunchState :: MunchState
emptyMunchState = []

munch :: T.Tree -> TempGen MunchState
munch t = execStateT (munchTree t) emptyMunchState

newPseudoReg :: Muncher Operand
newPseudoReg = liftM toReg (lift nextTemp)
  where
    toReg = Op_I . RegOp . VReg

emitInsn :: Insn -> Muncher ()
emitInsn i = do
  is <- get
  put $ is ++ [i]

-- Here comes the pattern matcher

munchTree :: T.Tree -> Muncher (Maybe Operand)
munchTree (T.Add t0 t1) = do
  (Just rand0) <- munchTree t0
  (Just rand1) <- munchTree t1
  munchAdd rand0 rand1
  where
    munchAdd (Op_I (ImmOp i0))
             (Op_I (ImmOp i1)) = do
      return $ Just $ Op_I $ ImmOp $ i0 + i1

    munchAdd (Op_I (RegOp r0))
             (Op_I (ImmOp i0)) = do
      tempReg <- newPseudoReg
      emitInsn (Lea tempReg (Op_M (Address r0 Nothing Scale1 i0)))
      return $ Just tempReg

    munchAdd i0@(Op_I (ImmOp _))
             r0@(Op_I (RegOp _)) =
      munchAdd r0 i0

    munchAdd (Op_I (RegOp r0))
             (Op_I (RegOp r1)) = do
      tempReg <- newPseudoReg
      emitInsn (Lea tempReg (Op_M (Address r0 (Just r1) Scale1 0)))
      return $ Just tempReg

-- XXX: manual munch will produce good code at the expense of duplicated
-- code and harder maintainence cost... what to?
munchTree (T.Move (T.Leaf r0@(RegOp _))
                       (T.Add t1 t2)) = do
  (Just rand1) <- munchTree t1
  (Just rand2) <- munchTree t2
  munchMoveAdd rand1 rand2
  return Nothing
  where
    munchMoveAdd (Op_I (RegOp r1))
                 (Op_I (RegOp r2)) = do
      emitInsn (Lea (Op_I r0)
                    (Op_M (Address r1 (Just r2) Scale1 0)))

    munchMoveAdd (Op_I (RegOp r1))
                 (Op_I (ImmOp i2)) = do
      emitInsn (Lea (Op_I r0)
                    (Op_M (Address r1 Nothing Scale1 i2)))

    munchMoveAdd i1@(Op_I (ImmOp _))
                 r2@(Op_I (RegOp _)) = do
      munchMoveAdd r2 i1

    munchMoveAdd (Op_I (ImmOp i1))
                 (Op_I (ImmOp i2)) = do
      emitInsn (Mov (Op_I r0) (Op_I (ImmOp $ i1 + i2)))

munchTree (T.Move (T.Leaf r0@(RegOp _)) t1) = do
  (Just rand1) <- munchTree t1
  munchMove rand1
  return Nothing
  where
    munchMove i0@(Op_I (ImmOp _)) = do
      emitInsn (Mov (Op_I r0) i0)
    munchMove r1@(Op_I (RegOp _)) = do
      emitInsn (Mov (Op_I r0) r1)

munchTree (T.Move (T.Deref t0) t1) = do
  (Just rand0) <- munchTree t0
  (Just rand1) <- munchTree t1
  munchMove rand0 rand1
  return Nothing
  where
    munchMove (Op_I (RegOp r0))
              r1@(Op_I (RegOp _)) = do
      emitInsn (Mov (Op_M (Address r0 Nothing Scale1 0)) r1)
    munchMove (Op_I (RegOp r0))
              i0@(Op_I (ImmOp ival)) = do
      if isInt32 ival
        then do
          emitInsn (Mov (Op_M (Address r0 Nothing Scale1 0)) i0)
        else do
          tempReg <- newPseudoReg
          emitInsn (Mov tempReg i0)
          emitInsn (Mov (Op_M (Address r0 Nothing Scale1 0)) tempReg)

munchTree (T.Deref t) = do
  (Just rand) <- munchTree t
  munchDeref rand
  where
    munchDeref (Op_I (RegOp r)) = do
      tempReg <- newPseudoReg
      emitInsn (Mov tempReg (Op_M (Address r Nothing Scale1 0)))
      return (Just tempReg)

munchTree (T.Seq t0 t1) = do
  munchTree t0
  munchTree t1
  return Nothing

munchTree (T.Leaf op) = do
  return $ Just $ Op_I op

munchTree T.Nop = return Nothing

-- Helpers
isInt32 :: Int -> Bool
isInt32 i = floor (- 2 ** 31) <= i && i <= floor (2 ** 31 - 1)

