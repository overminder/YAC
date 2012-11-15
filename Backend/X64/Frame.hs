module Backend.X64.Frame (
  Frame(..),
  FrameGen,
  runFrameGen,
  insertProAndEpilogue,
  insertCallerSave,
  patchCalleeMovArg,
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
  recordCallArgCount,

  StorageType(..),
  isInStack,

  wordSize,
  argRegs,
  callerArgOps,
  calleeArgOps,
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
  funcArgs :: [Reg],
  mostCallArgCount :: Int
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
-- rsp is treated differently
isCallerSave r = (not $ isCalleeSave r) && r /= rsp

usableRegs = useMore ++ useLess
  where
    (useLess, useMore) = span isCalleeSave usableRegs'
    usableRegs' = filter (\x -> x `notElem` scratchRegs ++ argRegs) [rax, rbx,
      rcx, rdx, rdi, rsi, r8, r9, r10, r11, r12, r13, r14, r15]

regCount = length usableRegs

wordSize :: Int
wordSize = 8

callerArgOps :: [X64Op]
callerArgOps = map (X64Op_I . IROp_R) argRegs ++ stackArgGen
  where
    stackArgGen = map (X64Op_M . nthStackArg) [0..]
    nthStackArg nth = Address rsp Nothing Scale1 (IVal (wordSize * nth))

calleeArgOps :: Int -> [X64Op]
calleeArgOps nSaves = map (X64Op_I . IROp_R) argRegs ++ frameArgGen
  where
    frameArgGen = map (X64Op_M . nthFrameArg) [0..]
    nthFrameArg nth = Address rbp Nothing Scale1
                              (IVal (wordSize * (1 + nth + nSaves)))

empty :: Frame
empty = Frame {
  name = "test",
  vRegUses = Map.empty,
  stackLocGen = (-wordSize),
  frameSize = 0,
  mRegUses = [],
  freeRegs = usableRegs,
  tempRegs = scratchRegs,
  funcArgs = [],
  mostCallArgCount = 0
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

recordCallArgCount :: Int -> FrameGen ()
recordCallArgCount i = modify $ \st -> st {
  mostCallArgCount = max (mostCallArgCount st) i
}

getMostCallArgCount :: FrameGen Int
getMostCallArgCount = liftM mostCallArgCount get

-- patches

insertProAndEpilogue :: [Insn] -> FrameGen [Insn]
insertProAndEpilogue insnList = do
  calleeSaveUses <- liftM (filter isCalleeSave . mRegUses) get
  liftM concat $ forM insnList $ \insn -> do
    case insn of
      (PInsn InsertPrologue) -> do
        frameSizeNeeded <- liftM frameSize get
        argStackNeeded <- liftM (\x -> wordSize * if x > length argRegs
                                                    then x - length argRegs
                                                    else 0)
                                getMostCallArgCount
        let spaceNeeded = frameSizeNeeded + argStackNeeded
        let saveInsns = map (Push . X64Op_I . IROp_R) calleeSaveUses
            enterInsns = [Push (X64Op_I (IROp_R rbp)),
                          Mov (X64Op_I (IROp_R rbp))
                              (X64Op_I (IROp_R rsp)) NormalMov]
            frameAdjInsns = if spaceNeeded /= 0
                              then [Sub (X64Op_I (IROp_R rsp))
                                        (X64Op_I (IROp_I (IVal spaceNeeded)))]
                              else []
        return $ saveInsns ++ enterInsns ++ frameAdjInsns
      (PInsn InsertEpilogue) -> do
        let restoreInsns = map (Pop . X64Op_I . IROp_R)
                               (reverse calleeSaveUses)
            leaveInsns = [Mov (X64Op_I (IROp_R rsp))
                              (X64Op_I (IROp_R rbp)) NormalMov,
                          Pop (X64Op_I (IROp_R rbp))]
        return $ leaveInsns ++ restoreInsns
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

patchCalleeMovArg :: [Insn] -> FrameGen [Insn]
patchCalleeMovArg insnList = do
  calleeSaveUses <- liftM (filter isCalleeSave . mRegUses) get
  let nSaves = 1 + length calleeSaveUses -- including rbp
  forM insnList $ \insn -> do
    case insn of
      (Mov dest (X64Op_M (Address base index scale (IVal disp)))
           CalleeMovArg) -> do
        let newDisp = disp + wordSize * nSaves
        return $ Mov dest (X64Op_M (Address base index scale (IVal newDisp)))
                     CalleeMovArg
      _ -> return insn

formatOutput :: GasSyntax a => [a] -> FrameGen String
formatOutput insnList =
  let writeLn s = tell s >> tell "\n"
   in execWriterT $ do
        funcName <- lift $ liftM name get
        writeLn $ ".global " ++ funcName
        writeLn $ funcName ++ ":"
        forM_ insnList $ \insn -> do
          writeLn $ gasShow insn

