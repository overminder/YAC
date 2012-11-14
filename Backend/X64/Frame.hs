module Backend.X64.Frame (
  Frame(..),
  FrameGen,
  runFrameGen,
  insertProAndEpilogue,
  insertCallerSave,
  formatOutput,

  nextTemp,
  newStackLoc,
  setStorageType,
  getStorageType,
  allocReg,
  freeReg,
  allocTempReg,
  restoreTempRegs,
  setFuncName,
  setFuncArgs,

  StorageType(..),
  isInStack,

  wordSize,
  argOps,
  regCount,
) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
import qualified Backend.IR.Temp as Temp
import Backend.X64.Insn
import Backend.X64.DataFlow

data Frame = Frame {
  name :: String,
  vRegUses :: Map Reg StorageType,
  stackLocGen :: Int,
  frameSize :: Int,
  mRegUses :: [Reg],
  freeRegs :: [Reg],
  tempRegs :: [Reg],
  funcArgs :: [Reg]
}

type FrameGen = StateT Frame Temp.TempGen

data StorageType = InReg Reg
                 | InStack StackLoc
  deriving (Show, Eq)

isInStack :: StorageType -> Bool
isInStack (InStack _) = True
isInStack _ = False

type StackLoc = Int
type StackLocGen = Int

-- Defs

-- Note: none of the scratch regs are callee-saved.
-- we need at most 3 scratch regs since a x64 insn can at most contain 3 regs
scratchRegs = [rax, rcx, rdx]
isScratchReg r = r `elem` scratchRegs

argRegs = [rdi, rsi, rdx, rcx, r8, r9]

calleeSaveRegs = [rbp, rbx, r12, r13, r14, r15]
isCalleeSave r = r `elem` calleeSaveRegs
isCallerSave r = not $ isCalleeSave r

usableRegs = useMore ++ useLess
  where
    (useLess, useMore) = span isCalleeSave usableRegs'
    usableRegs' = filter (\x -> x `notElem` scratchRegs ++ argRegs) [rax, rbx,
      rcx, rdx, rdi, rsi, r8, r9, r10, r11, r12, r13, r14, r15]

regCount = length usableRegs

wordSize :: Int
wordSize = 8

argOps :: [X64Op]
argOps = map (X64Op_I . IROp_R) argRegs ++ stackArgGen
  where
    stackArgGen = map (X64Op_M . nthStackArg) [0..]
    nthStackArg n = Address rsp Nothing Scale1 (wordSize * n)

empty :: Frame
empty = Frame {
  name = "test",
  vRegUses = Map.empty,
  stackLocGen = (-wordSize),
  frameSize = 0,
  mRegUses = [],
  freeRegs = usableRegs,
  tempRegs = scratchRegs,
  funcArgs = []
}

nextTemp :: FrameGen Int
nextTemp = lift Temp.nextTemp

useReg :: Reg -> FrameGen ()
useReg r = modify $ \st -> st {
  mRegUses = [r] `List.union` mRegUses st
}

runFrameGen :: FrameGen a -> Temp.TempGen a
runFrameGen m = evalStateT m empty

newStackLoc :: FrameGen StackLoc
newStackLoc = do
  newLoc <- liftM stackLocGen get
  modify $ \st -> st {
    stackLocGen = newLoc - wordSize,
    frameSize = wordSize + frameSize st
  }
  return newLoc

setStorageType :: Reg -> StorageType -> FrameGen ()
setStorageType r sty = do
  regMap <- liftM vRegUses get
  modify $ \st -> st {
    vRegUses = Map.insert r sty regMap
  }

getStorageType :: Reg -> FrameGen StorageType
getStorageType r = do
  regMap <- liftM vRegUses get
  let (Just sty) = Map.lookup r regMap
  return sty

--hasStorage :: Reg -> FrameGen StorageType

setFuncName :: String -> FrameGen ()
setFuncName s = modify $ \st -> st {
  name = s
}

setFuncArgs :: [Reg] -> FrameGen ()
setFuncArgs rs = modify $ \st -> st {
  funcArgs = rs
}

allocReg :: FrameGen Reg
allocReg = do
  (r:frs) <- liftM freeRegs get
  modify $ \st -> st {
    freeRegs = frs
  }
  useReg r
  return r

freeReg :: Reg -> FrameGen ()
freeReg r = do
  frs <- liftM freeRegs get
  modify $ \st -> st {
    freeRegs = (r:frs)
  }

allocTempReg :: FrameGen Reg
allocTempReg = do
  (r:trs) <- liftM tempRegs get
  modify $ \st -> st {
    tempRegs = trs
  }
  useReg r
  return r

restoreTempRegs :: FrameGen ()
restoreTempRegs = do
  modify $ \st -> st {
    tempRegs = scratchRegs
  }

insertProAndEpilogue :: [Insn] -> FrameGen [Insn]
insertProAndEpilogue insnList = do
  liftM concat $ forM insnList $ \insn -> do
    case insn of
      (PInsn InsertPrologue) -> do
        spaceNeeded <- liftM frameSize get
        mRegs <- liftM mRegUses get
        let saveInsns = map (Push . X64Op_I . IROp_R)
                            (filter isCalleeSave mRegs)
            enterInsns = [Push (X64Op_I (IROp_R rbp)),
                          Mov (X64Op_I (IROp_R rbp)) (X64Op_I (IROp_R rsp))]
            frameAdjInsns = if spaceNeeded /= 0
                              then [Sub (X64Op_I (IROp_R rsp))
                                        (X64Op_I (IROp_I spaceNeeded))]
                              else []
        return $ enterInsns ++ frameAdjInsns ++ saveInsns
      (PInsn InsertEpilogue) -> do
        mRegs <- liftM mRegUses get
        let restoreInsns = map (Pop . X64Op_I . IROp_R)
                               (reverse $ filter isCalleeSave mRegs)
            leaveInsns = [Mov (X64Op_I (IROp_R rsp)) (X64Op_I (IROp_R rbp)),
                          Pop (X64Op_I (IROp_R rbp))]
        return $ restoreInsns ++ leaveInsns
      _ -> return [insn]

-- TODO: improve the algorithm
insertCallerSave :: [DFInsn] -> FrameGen [Insn]
insertCallerSave insnList = do
  liftM concat $ forM insnList $ \(DFInsn insn _ (Liveness lvIn _)) -> do
    case insn of
      (Call _) -> do
        let callerSaves = filter
              (\x -> isCallerSave x && (not $ isScratchReg x)) lvIn
        --forM callerSaves $ \r -> do
        --  case
            saveInsns = map (Push . X64Op_I . IROp_R) callerSaves
            restoreInsns = map (Pop . X64Op_I . IROp_R) (reverse callerSaves)
        return $ saveInsns ++ [insn] ++ restoreInsns
      _ -> return [insn]

formatOutput :: GasSyntax a => [a] -> FrameGen String
formatOutput insnList =
  let writeLn s = tell s >> tell "\n"
   in execWriterT $ do
        funcName <- lift $ liftM name get
        writeLn $ ".global " ++ funcName
        writeLn $ funcName ++ ":"
        forM_ insnList $ \insn -> do
          writeLn $ gasShow insn

