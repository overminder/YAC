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

type MunchGen = StateT MunchState TempGen

type MunchState = [Insn]

emptyMunchState :: MunchState
emptyMunchState = []

munch :: T.Tree -> TempGen MunchState
munch t = execStateT (munch' t) emptyMunchState

munch' = munchTree

newVReg :: MunchGen X64Op
newVReg = liftM toReg (lift nextTemp)
  where
    toReg = X64Op_I . IROp_R . VReg

newLabel :: MunchGen Label
newLabel = do
  i <- lift nextTemp
  return $ IntLabel i

emitInsn :: Insn -> MunchGen ()
emitInsn i = do
  is <- get
  put $ is ++ [i]

-- Here comes the pattern matcher
-- XXX: manual munch will produce good code at the expense of duplicated
-- code and harder maintainence cost... what to?

munchTree :: T.Tree -> MunchGen (Maybe X64Op)
munchTree t = case t of
  --
  (T.Add t0 t1) -> do
    (Just rand0) <- munchTree t0
    (Just rand1) <- munchTree t1
    munchAdd rand0 rand1
    where
      munchAdd (X64Op_I (IROp_I i0))
               (X64Op_I (IROp_I i1)) = do
        return $ Just $ X64Op_I $ IROp_I $ i0 + i1

      munchAdd (X64Op_I (IROp_R r0))
               (X64Op_I (IROp_I i0)) = do
        tempReg <- newVReg
        emitInsn (Lea tempReg (X64Op_M (Address r0 Nothing Scale1 i0)))
        return $ Just tempReg

      munchAdd i0@(X64Op_I (IROp_I _))
               r0@(X64Op_I (IROp_R _)) =
        munchAdd r0 i0

      munchAdd (X64Op_I (IROp_R r0))
               (X64Op_I (IROp_R r1)) = do
        tempReg <- newVReg
        emitInsn (Lea tempReg (X64Op_M (Address r0 (Just r1) Scale1 0)))
        return $ Just tempReg

  (T.Move (T.Leaf r0@(IROp_R _)) (T.Add t1 t2)) -> do
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

  (T.Move (T.Leaf r0@(IROp_R _)) t1) -> do
    (Just rand1) <- munchTree t1
    munchMove rand1
    return Nothing
    where
      munchMove i0@(X64Op_I (IROp_I _)) = do
        emitInsn (Mov (X64Op_I r0) i0)
      munchMove r1@(X64Op_I (IROp_R _)) = do
        emitInsn (Mov (X64Op_I r0) r1)

  (T.Move (T.Deref t0) t1) -> do
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
            tempReg <- newVReg
            emitInsn (Mov tempReg i0)
            emitInsn (Mov (X64Op_M (Address r0 Nothing Scale1 0)) tempReg)

  (T.Deref t) -> do
    (Just rand) <- munchTree t
    munchDeref rand
    where
      munchDeref (X64Op_I (IROp_R r)) = do
        tempReg <- newVReg
        emitInsn (Mov tempReg (X64Op_M (Address r Nothing Scale1 0)))
        return (Just tempReg)

  (T.If t0 t1 t2) -> do
    retVal@(X64Op_I retValIR) <- newVReg
    elseLabel <- newLabel
    endLabel <- newLabel
    (Just v0) <- (munchTree t0)
    r0 <- ensureReg v0
    emitInsn (Cmp r0 (X64Op_I $ IROp_I 0))
    emitInsn (J Eq elseLabel)
    munchTree (T.Move (T.Leaf retValIR) t1)
    emitInsn (Jmp endLabel)
    emitInsn (BindLabel elseLabel)
    munchTree (T.Move (T.Leaf retValIR) t2)
    emitInsn (BindLabel endLabel)
    return $ Just retVal

  (T.Seq t0 t1) -> do
    r0 <- munchTree t0
    case t1 of
      T.Nop -> return r0
      _ -> munchTree t1

  (T.Leaf op) -> do
    return $ Just $ X64Op_I op

  -- needs better handling
  (T.Call (T.Leaf (IROp_L name)) argTrees) -> do
    forM_ (zip argTrees argRegs) $ \(t, dest) -> do
      (Just r) <- munchTree t
      r <- ensureReg r
      emitInsn $ Mov dest r
    emitInsn $ Call (StringLabel name)
    return $ Just (X64Op_I (IROp_R rax))

  (T.Return t) -> do
    munchTree (T.Move (T.Leaf (IROp_R rax)) t)
    -- epilogue
    emitInsn (Mov (X64Op_I (IROp_R rsp)) (X64Op_I (IROp_R rbp)))
    emitInsn (Pop (X64Op_I (IROp_R rbp)))
    emitInsn Ret
    return Nothing

  T.Nop -> error "not reached"

ensureReg :: X64Op -> MunchGen X64Op
ensureReg op = case op of
  (X64Op_I (IROp_R _)) -> return op
  (X64Op_I (IROp_I val)) -> do
    vReg <- newVReg
    emitInsn (Mov vReg op)
    return vReg
  (X64Op_M addr) -> do
    vReg <- newVReg
    emitInsn (Mov vReg op)
    return vReg

-- Helpers
isInt32 :: Int -> Bool
isInt32 i = floor (- 2 ** 31) <= i && i <= floor (2 ** 31 - 1)

argRegs :: [X64Op]
argRegs = map (X64Op_I . IROp_R) [rdi, rsi, rdx, rcx, r8, r9] ++ stackArgGen
  where
    stackArgGen = map (X64Op_M . nthStackArg) [0..]
    nthStackArg n = Address rsp Nothing Scale1 (wordSize * n)

