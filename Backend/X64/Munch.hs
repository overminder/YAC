module Backend.X64.Munch (
  munch
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
import qualified Backend.IR.Tree as T
import Backend.IR.Temp
import Backend.X64.Insn

type Muncher = StateT MunchState TempGen

type MunchState = [Insn]

emptyMunchState :: MunchState
emptyMunchState = []

munch :: T.Tree -> TempGen MunchState
munch t = execStateT (munchTree t) emptyMunchState

newPseudoReg :: Muncher X64Op
newPseudoReg = liftM toReg (lift nextTemp)
  where
    toReg = X64Op_I . IROp_R . VReg

emitInsn :: Insn -> Muncher ()
emitInsn i = do
  is <- get
  put $ is ++ [i]

-- Here comes the pattern matcher

munchTree :: T.Tree -> Muncher (Maybe X64Op)
munchTree (T.Add t0 t1) = do
  (Just rand0) <- munchTree t0
  (Just rand1) <- munchTree t1
  munchAdd rand0 rand1
  where
    munchAdd (X64Op_I (IROp_I i0))
             (X64Op_I (IROp_I i1)) = do
      return $ Just $ X64Op_I $ IROp_I $ i0 + i1

    munchAdd (X64Op_I (IROp_R r0))
             (X64Op_I (IROp_I i0)) = do
      tempReg <- newPseudoReg
      emitInsn (Lea tempReg (X64Op_M (Address r0 Nothing Scale1 i0)))
      return $ Just tempReg

    munchAdd i0@(X64Op_I (IROp_I _))
             r0@(X64Op_I (IROp_R _)) =
      munchAdd r0 i0

    munchAdd (X64Op_I (IROp_R r0))
             (X64Op_I (IROp_R r1)) = do
      tempReg <- newPseudoReg
      emitInsn (Lea tempReg (X64Op_M (Address r0 (Just r1) Scale1 0)))
      return $ Just tempReg

-- XXX: manual munch will produce good code at the expense of duplicated
-- code and harder maintainence cost... what to?
munchTree (T.Move (T.Leaf r0@(IROp_R _))
                       (T.Add t1 t2)) = do
  (Just rand1) <- munchTree t1
  (Just rand2) <- munchTree t2
  munchMoveAdd rand1 rand2
  return Nothing
  where
    munchMoveAdd (X64Op_I (IROp_R r1))
                 (X64Op_I (IROp_R r2)) = do
      emitInsn (Lea (X64Op_I r0)
                    (X64Op_M (Address r1 (Just r2) Scale1 0)))

    munchMoveAdd (X64Op_I (IROp_R r1))
                 (X64Op_I (IROp_I i2)) = do
      emitInsn (Lea (X64Op_I r0)
                    (X64Op_M (Address r1 Nothing Scale1 i2)))

    munchMoveAdd i1@(X64Op_I (IROp_I _))
                 r2@(X64Op_I (IROp_R _)) = do
      munchMoveAdd r2 i1

    munchMoveAdd (X64Op_I (IROp_I i1))
                 (X64Op_I (IROp_I i2)) = do
      emitInsn (Mov (X64Op_I r0) (X64Op_I (IROp_I $ i1 + i2)))

munchTree (T.Move (T.Leaf r0@(IROp_R _)) t1) = do
  (Just rand1) <- munchTree t1
  munchMove rand1
  return Nothing
  where
    munchMove i0@(X64Op_I (IROp_I _)) = do
      emitInsn (Mov (X64Op_I r0) i0)
    munchMove r1@(X64Op_I (IROp_R _)) = do
      emitInsn (Mov (X64Op_I r0) r1)

munchTree (T.Move (T.Deref t0) t1) = do
  (Just rand0) <- munchTree t0
  (Just rand1) <- munchTree t1
  munchMove rand0 rand1
  return Nothing
  where
    munchMove (X64Op_I (IROp_R r0))
              r1@(X64Op_I (IROp_R _)) = do
      emitInsn (Mov (X64Op_M (Address r0 Nothing Scale1 0)) r1)
    munchMove (X64Op_I (IROp_R r0))
              i0@(X64Op_I (IROp_I ival)) = do
      if isInt32 ival
        then do
          emitInsn (Mov (X64Op_M (Address r0 Nothing Scale1 0)) i0)
        else do
          tempReg <- newPseudoReg
          emitInsn (Mov tempReg i0)
          emitInsn (Mov (X64Op_M (Address r0 Nothing Scale1 0)) tempReg)

munchTree (T.Deref t) = do
  (Just rand) <- munchTree t
  munchDeref rand
  where
    munchDeref (X64Op_I (IROp_R r)) = do
      tempReg <- newPseudoReg
      emitInsn (Mov tempReg (X64Op_M (Address r Nothing Scale1 0)))
      return (Just tempReg)

munchTree (T.Seq t0 t1) = do
  munchTree t0
  munchTree t1
  return Nothing

munchTree (T.Leaf op) = do
  return $ Just $ X64Op_I op

munchTree T.Nop = return Nothing

-- Helpers
isInt32 :: Int -> Bool
isInt32 i = floor (- 2 ** 31) <= i && i <= floor (2 ** 31 - 1)

