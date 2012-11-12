module Backend.X64.RegAlloc (
  getLiveRange,
  alloc
) where

import Control.Monad.State
import Control.Monad.Maybe
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
import Backend.IR.Temp
import Backend.X64.Insn
import Backend.X64.DataFlow

-- X64 reg spec
allRegs = map MReg ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi",
    "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

[rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]
  = allRegs

usableRegs = [rax, rbx, rcx, rdx, rdi, rsi,
    r8, r9, r10, r11, r12, r13, r14, r15]

regCount = length usableRegs

-- State type
data AllocState = AllocState {
  freeRegs :: [Reg],
  activeIntervals :: [Interval],
  registers :: Map Interval Reg,
  location :: Map Interval StackLoc,
  stackLocGen :: StackLocGen,
  stackSize :: Int
}

-- Reg start end
type Interval = (Reg, Int, Int)

type AllocGen = StateT AllocState TempGen

emptyAllocState :: AllocState
emptyAllocState = AllocState {
  freeRegs = usableRegs,
  activeIntervals = [],
  registers = Map.empty,
  location = Map.empty,
  stackLocGen = 0,
  stackSize = 0
}

type StackLoc = Int
type StackLocGen = Int

wordSize :: Int
wordSize = 8

newStackLoc :: AllocGen StackLoc
newStackLoc = do
  newLoc <- liftM ((-)wordSize . stackLocGen) get
  modify $ \st -> st {
    stackLocGen = newLoc,
    stackSize = wordSize + stackSize st
  }
  return newLoc

setRegister :: Interval -> Reg -> AllocGen ()
setRegister iVal r = do
  modify $ \st -> st {
    registers = Map.insert iVal r (registers st)
  }

getRegister :: Interval -> AllocGen Reg
getRegister iVal = do
  rMap <- liftM registers get
  let (Just r) = Map.lookup iVal rMap
  return r

setLocation :: Interval -> StackLoc -> AllocGen ()
setLocation iVal loc = do
  modify $ \st -> st {
    location = Map.insert iVal loc (location st)
  }

addActiveInterval :: Interval -> AllocGen ()
addActiveInterval newVal = do
  actVals <- liftM activeIntervals get
  modify $ \st -> st {
    activeIntervals = List.sortBy compareInterval (newVal:actVals)
  }

removeActiveInterval :: Interval -> AllocGen ()
removeActiveInterval rmVal = do
  actVals <- liftM activeIntervals get
  modify $ \st -> st {
    activeIntervals = List.sortBy compareInterval (List.delete rmVal actVals)
  }

alloc :: [Insn] -> [Liveness] -> TempGen [Insn]
alloc insnList lvs = evalStateT res emptyAllocState
  where
    lvMap = getLiveRange lvs
    sortedLvs = sortedLiveRange lvMap
    res = do
      forM_ sortedLvs $ \val -> do
        expireOldIntervals val
        activeLen <- liftM (length . activeIntervals) get
        if activeLen == regCount
          then spillAtInterval val
          else do
            r <- allocReg
            addActiveInterval val
      mapM replaceVReg (zip [0..] insnList)

replaceVReg :: (Int, Insn) -> AllocGen Insn
replaceVReg (idx, insn) = do
  let oldRegs = regsOfInsn insn
      newRegs = oldRegs
      newInsn = setRegsOfInsn newRegs insn
  return newInsn

expireOldIntervals :: Interval -> AllocGen ()
expireOldIntervals val = do
  actVals <- liftM activeIntervals get
  expireOldIntervals' actVals val

expireOldIntervals' :: [Interval] -> Interval -> AllocGen ()
expireOldIntervals' [] _ = return ()
expireOldIntervals' (j@(jReg, jStart, jEnd):vs) i@(iReg, iStart, iEnd) = do
  if (jEnd > iStart)
    then return () -- we are done
    else do
      removeActiveInterval j
      freeReg jReg
      expireOldIntervals' vs i

getLastInterval :: AllocGen Interval
getLastInterval = liftM (last . activeIntervals) get

spillAtInterval :: Interval -> AllocGen ()
spillAtInterval iVal@(iReg, iStart, iEnd) = do
  spill@(sReg, sStart, sEnd) <- getLastInterval
  if sEnd > iEnd
    then do
      r <- getRegister spill
      setRegister iVal r
      loc <- newStackLoc
      setLocation spill loc
      removeActiveInterval spill
      addActiveInterval iVal
    else do
      loc <- newStackLoc
      setLocation iVal loc
      
  return ()

allocReg :: AllocGen Reg
allocReg = do
  (r:frs) <- liftM freeRegs get
  modify $ \st -> st {
    freeRegs = frs
  }
  return r

freeReg :: Reg -> AllocGen ()
freeReg r = do
  frs <- liftM freeRegs get
  modify $ \st -> st {
    freeRegs = (r:frs)
  }

-- Internally used for live range calculating
type LiveMap = Map Reg (Int, Int)

-- (VReg, MReg)
type ActiveReg = (Reg, Reg)

compareInterval :: Interval -> Interval -> Ordering
compareInterval (_, f0, _) (_, f1, _) = compare f0 f1

-- result is sorted in increasing start point order
sortedLiveRange :: LiveMap -> [Interval]
sortedLiveRange lvMap = List.sortBy compareInterval ranges
  where
    ranges = map toInterval (Map.toList lvMap)
    toInterval (r, (f, t)) = (r, f, t)

getLiveRange :: [Liveness] -> LiveMap
getLiveRange lvs = foldl addInterval Map.empty (zip [0..] lvs)
  where
    addInterval :: LiveMap -> (Int, Liveness) -> LiveMap
    addInterval lvMap (idx, (Liveness regs)) = newLvMap
      where
        newLvMap = foldr combine lvMap regs
        combine reg lvMap = case Map.lookup reg lvMap of
          (Just (frm, to)) -> Map.insert reg (frm, idx) lvMap
          Nothing -> Map.insert reg (idx, idx) lvMap

