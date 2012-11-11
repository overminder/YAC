module Backend.X64.RegAlloc (
  getLiveRange
) where

import Control.Monad.State
import Control.Monad.Maybe
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.Oprnd
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
  activeIntervals :: [Interval]
}

-- Reg start end
type Interval = (Reg, Int, Int)

type AllocGen = StateT AllocState TempGen

emptyAllocState :: AllocState
emptyAllocState = AllocState {
  freeRegs = usableRegs,
  activeIntervals = []
}

addActiveInterval :: Interval -> AllocGen ()
addActiveInterval newVal = do
  actVals <- liftM activeIntervals get
  modify $ \st -> st {
    activeIntervals = sortBy compareInterval (newVal:actVals)
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
      return []

expireOldIntervals :: Interval -> AllocGen ()
expireOldIntervals val = undefined

spillAtInterval :: Interval -> AllocGen ()
spillAtInterval val@(iReg, iStart, iEnd) = do
  runMaybeT $ do
    actInts <- liftM activeIntervals (lift get)
    forM actInts $ \(jReg, jStart, jEnd) -> do
      guard (jEnd > iStart) -- ??
  return ()

allocReg :: AllocGen Reg
allocReg = do
  (r:frs) <- liftM freeRegs get
  modify $ \st -> st {
    freeRegs = frs
  }
  return r

-- Internally used for live range calculating
type LiveMap = Map Reg (Int, Int)

-- (VReg, MReg)
type ActiveReg = (Reg, Reg)

compareInterval :: Interval -> Interval -> Ordering
compareInterval (_, f0, _) (_, f1, _) = compare f0 f1

-- result is sorted in increasing start point order
sortedLiveRange :: LiveMap -> [Interval]
sortedLiveRange lvMap = sortBy compareInterval ranges
  where
    ranges = map (\(r, (f, t)) -> (r, f, t)) (Map.toList lvMap)

getLiveRange :: [Liveness] -> LiveMap
getLiveRange lvs = foldl addInterval Map.empty (zip [0..] lvs)

addInterval :: LiveMap -> (Int, Liveness) -> LiveMap
addInterval lvMap (idx, (Liveness regs)) = newLvMap
  where
    newLvMap = foldr combine lvMap regs
    combine reg lvMap = case Map.lookup reg lvMap of
      (Just (frm, to)) -> Map.insert reg (frm, idx) lvMap
      Nothing -> Map.insert reg (idx, idx) lvMap

