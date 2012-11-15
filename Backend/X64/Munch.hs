module Backend.X64.Munch (
  munch
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
import qualified Backend.IR.Tree as T
import Backend.X64.Insn
import qualified Backend.X64.Frame as F

type MunchGen = WriterT [Insn] F.FrameGen

munch :: T.Tree -> F.FrameGen [Insn]
munch t = execWriterT (munch' t)

munch' :: T.Tree -> MunchGen ()
munch' t = do
  emitInsn $ PInsn InsertPrologue
  funcArgs <- lift $ liftM F.funcArgs get
  emitInsns $ map (\(src, dest) -> Mov (X64Op_I $ IROp_R dest) src CalleeMovArg)
                  (zip (F.calleeArgOps 0) funcArgs)
  munchTree t
  return ()

newVReg :: MunchGen X64Op
newVReg = liftM toReg (lift F.nextTemp)
  where
    toReg = X64Op_I . IROp_R . VReg

newLabel :: MunchGen Label
newLabel = do
  i <- lift F.nextTemp
  return $ IntLabel i

emitInsns :: [Insn] -> MunchGen ()
emitInsns = tell

emitInsn :: Insn -> MunchGen ()
emitInsn i = emitInsns [i]

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

  (T.Sub t0 t1) -> do
    (Just rand0) <- munchTree t0
    (Just rand1) <- munchTree t1
    munchSub rand0 rand1
    where
      munchSub (X64Op_I (IROp_I i0))
               (X64Op_I (IROp_I i1)) = do
        return $ Just $ X64Op_I $ IROp_I $ i0 - i1

      munchSub (X64Op_I r0@(IROp_R _))
               (X64Op_I (IROp_I i0)) = do
        -- uses munchAdd
        munchTree $ T.Add (T.Leaf r0) (T.Leaf (IROp_I (-i0)))

      munchSub i0@(X64Op_I (IROp_I _))
               r0@(X64Op_I (IROp_R _)) = do
        tempReg <- newVReg
        emitInsn $ Mov tempReg i0 NormalMov
        emitInsn $ Sub tempReg r0
        return $ Just tempReg

      munchSub r0@(X64Op_I (IROp_R _))
               r1@(X64Op_I (IROp_R _)) = do
        tempReg <- newVReg
        emitInsn $ Mov tempReg r0 NormalMov
        emitInsn $ Sub tempReg r1
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
        emitInsn $ Mov (X64Op_I r0) (X64Op_I (IROp_I $ i1 + i2)) NormalMov

  (T.Move (T.Leaf r0@(IROp_R _)) t1) -> do
    (Just rand1) <- munchTree t1
    munchMove rand1
    return Nothing
    where
      munchMove i0@(X64Op_I (IROp_I _)) = do
        emitInsn $ Mov (X64Op_I r0) i0 NormalMov
      munchMove r1@(X64Op_I (IROp_R _)) = do
        emitInsn $ Mov (X64Op_I r0) r1 NormalMov

  (T.Move (T.Deref t0) t1) -> do
    (Just rand0) <- munchTree t0
    (Just rand1) <- munchTree t1
    munchMove rand0 rand1
    return Nothing
    where
      munchMove (X64Op_I (IROp_R r0))
                r1@(X64Op_I (IROp_R _)) = do
        emitInsn $ Mov (X64Op_M (Address r0 Nothing Scale1 0)) r1 NormalMov
      munchMove (X64Op_I (IROp_R r0))
                i0@(X64Op_I (IROp_I ival)) = do
        if isInt32 ival
          then do
            emitInsn $ Mov (X64Op_M (Address r0 Nothing Scale1 0)) i0 NormalMov
          else do
            tempReg <- newVReg
            emitInsn $ Mov tempReg i0 NormalMov
            emitInsn $ Mov (X64Op_M (Address r0 Nothing Scale1 0))
                           tempReg NormalMov

  (T.Deref t) -> do
    (Just rand) <- munchTree t
    munchDeref rand
    where
      munchDeref (X64Op_I (IROp_R r)) = do
        tempReg <- newVReg
        emitInsn $ Mov tempReg (X64Op_M (Address r Nothing Scale1 0)) NormalMov
        return (Just tempReg)

  -- using (if (cmp ...) ...)
  (T.Compare lhs rhs cond) -> do
    let one = T.Leaf $ IROp_I 1
        zero = T.Leaf $ IROp_I 0
    munchTree (T.If t one zero)

  (T.If t0 t1 t2) -> do
    retVal@(X64Op_I retValIR) <- newVReg
    elseLabel <- newLabel
    endLabel <- newLabel
    case t0 of
      (T.Compare lhs rhs cond) -> do
        (Just vlhs) <- munchTree lhs
        rlhs <- ensureReg vlhs
        (Just vrhs) <- munchTree rhs
        rrhs <- ensureReg vrhs
        emitInsn $ Cmp rlhs rrhs
        emitInsn $ J (T.reverseCond cond) elseLabel
      _ -> do
        (Just v0) <- munchTree t0
        r0 <- ensureReg v0
        emitInsn (Cmp r0 (X64Op_I $ IROp_I 0))
        emitInsn (J T.Eq elseLabel)
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

  (T.Call (T.Leaf (IROp_L name)) argTrees tailp) -> do
    -- Firstly evaluate all args
    let nArgs = length argTrees
    lift $ F.recordCallArgCount nArgs
    argValues <- forM argTrees munchTree
    -- Then put them into the arg pos
    forM (zip argValues F.callerArgOps) $ \(Just r, dest) -> do
      r <- ensureReg r
      emitInsn $ Mov dest r NormalMov
    case tailp of
      T.NormalCall -> do
        emitInsn $ Call (StringLabel name)
        -- Save the result into a non-temp register
        vReg <- newVReg
        emitInsn $ Mov vReg (X64Op_I $ IROp_R rax) NormalMov
        return $ Just vReg
      T.TailCall -> do
        if nArgs >= length F.argRegs
          then error $ "Munch: tailcall has too many args: " ++ show t
          else do
            emitInsn $ PInsn InsertEpilogue
            emitInsn $ Jmp (StringLabel name)
            return $ Just (X64Op_I $ IROp_R rax) -- Actually not reached

  (T.Return t) -> do
    munchTree (T.Move (T.Leaf (IROp_R rax)) t)
    emitInsn (PInsn InsertEpilogue)
    emitInsn Ret
    return Nothing

  T.Nop -> error "not reached"

ensureReg :: X64Op -> MunchGen X64Op
ensureReg op = case op of
  (X64Op_I (IROp_R _)) -> return op
  (X64Op_I (IROp_I val)) -> do
    vReg <- newVReg
    emitInsn (Mov vReg op NormalMov)
    return vReg
  (X64Op_M addr) -> do
    vReg <- newVReg
    emitInsn (Mov vReg op NormalMov)
    return vReg

-- Helpers
isInt32 :: Int -> Bool
isInt32 i = floor (- 2 ** 31) <= i && i <= floor (2 ** 31 - 1)

