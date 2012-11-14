module Backend.X64.RegAlloc (
  getLiveRange,
  alloc
) where

import Control.Monad.State
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
import Backend.X64.DataFlow
import Backend.X64.Insn
import qualified Backend.X64.Frame as F

-- State type
newtype AllocState = AllocState {
  activeIntervals :: [Interval]
}
  deriving (Show)

-- Reg start end
type Interval = (Reg, Int, Int)

type AllocGen = StateT AllocState F.FrameGen

emptyAllocState :: AllocState
emptyAllocState = AllocState {
  activeIntervals = []
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

alloc :: [DFInsn] -> F.FrameGen [Insn]
alloc insnList = evalStateT (alloc' insnList) emptyAllocState

alloc' :: [DFInsn] -> AllocGen [Insn]
alloc' insnList = do
  let lvMap = getLiveRange lvs
      lvs = map (\(DFInsn _ _ lv) -> lv) insnList
      sortedLvs = sortedLiveRange lvMap
  forM_ sortedLvs $ \iVal@(iReg, _, _) -> do
    expireOldIntervals iVal
    activeLen <- liftM (length . activeIntervals) get
    if activeLen == F.regCount
      then spillAtInterval iVal
      else do
        r <- lift F.allocReg
        addActiveInterval iVal
        lift $ F.setStorageType iReg (F.InReg r)
  liftM concat $ mapM materialize insnList

-- Replaces virtual registers in insn and adds spilling instructions if needed.
-- XXX Also does dead-code elimination when possible.
materialize :: DFInsn -> AllocGen [Insn]
materialize (DFInsn insn du lv) = do
  regMap <- lift $ liftM F.vRegUses get
  if any (flip Map.notMember regMap) (getUse du ++ getDef du)
    then return [] {- Test if any of the defined (but not used later)
                      or used (though IMPOSSIBLE) VReg is present
                      in this insn. If so, then this insn is doing
                      nothing and should be removed (DCE).

                      XXX: actually this opt does not belong to here.. -}
    else do
      let getItem r = case Map.lookup r regMap of
            (Just result) -> result
            Nothing -> error $ "Cannot find " ++ show r -- Shall never happen
          loadRegs = filter (F.isInStack . getItem) (getUse du)
          storeRegs = filter (F.isInStack . getItem) (getDef du)
          totalRegs = loadRegs `List.union` storeRegs
      --
      tempMap <- liftM Map.fromList $ forM totalRegs $ \r -> do
        let (F.InStack loc) = getItem r
        tempReg <- lift F.allocTempReg
        let loadInsn = Mov (X64Op_I (IROp_R tempReg))
                           (X64Op_M (Address rbp Nothing Scale1 loc))
            storeInsn = Mov (X64Op_M (Address rbp Nothing Scale1 loc))
                            (X64Op_I (IROp_R tempReg))
        return (r, (tempReg, loadInsn, storeInsn))
      --
      let loads = map (\r -> chooseLoad (tempMap Map.! r)) loadRegs
          stores = map (\r -> chooseStore (tempMap Map.! r)) storeRegs
          chooseTemp (x, _, _) = x
          chooseLoad (_, x, _) = x
          chooseStore (_, _, x) = x
          mRegForVReg reg = case reg of
            mReg@(MReg _) -> mReg
            vReg@(VReg _) -> case Map.lookup vReg tempMap of
              (Just mRegInfo) -> chooseTemp mRegInfo
              Nothing -> case Map.lookup vReg regMap of
                (Just (F.InReg mReg)) -> mReg
                _ -> error "RegAlloc.materialize: Internal Error"
      
      lift F.restoreTempRegs
      return $ loads ++ [replaceVReg mRegForVReg insn] ++ stores

expireOldIntervals :: Interval -> AllocGen ()
expireOldIntervals val = do
  actVals <- liftM activeIntervals get
  expireOldIntervals' actVals val

expireOldIntervals' :: [Interval] -> Interval -> AllocGen ()
expireOldIntervals' [] _ = return ()
expireOldIntervals' (j@(jReg, jStart, jEnd):vs) i@(iReg, iStart, iEnd) = do
  if (jEnd >= iStart) {- Different from poletto (he uses >):
                         our live range overlaps. -}
    then return () -- we are done
    else do
      removeActiveInterval j
      (F.InReg jMachReg) <- lift $ F.getStorageType jReg
      lift $ F.freeReg jMachReg
      expireOldIntervals' vs i

getLastInterval :: AllocGen Interval
getLastInterval = liftM (last . activeIntervals) get

spillAtInterval :: Interval -> AllocGen ()
spillAtInterval iVal@(iReg, iStart, iEnd) = do
  spill@(sReg, sStart, sEnd) <- getLastInterval
  if sEnd > iEnd
    then do
      (F.InReg r) <- lift $ F.getStorageType sReg
      loc <- lift F.newStackLoc
      lift $ F.setStorageType sReg (F.InStack loc)
      lift $ F.setStorageType iReg (F.InReg r)
      removeActiveInterval spill
      addActiveInterval iVal
    else do
      loc <- lift F.newStackLoc
      lift $ F.setStorageType iReg (F.InStack loc)
  return ()

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
    addInterval lvMap (idx, Liveness lvIn lvOut) = newLvMap
      where
        newLvMap = foldr combine lvMap lvOut
        combine reg lvMap = case Map.lookup reg lvMap of
          (Just (frm, to)) -> Map.insert reg (frm, idx) lvMap
          Nothing -> Map.insert reg (idx, idx) lvMap

