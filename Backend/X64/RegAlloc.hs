module Backend.X64.RegAlloc (
  getLiveRange,
  alloc,
  runAlloc
) where

import Control.Monad.State
import Control.Monad.Maybe
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
import Backend.IR.Temp
import Backend.X64.DataFlow
import Backend.X64.Insn

scratchRegs = [r13, r14, r15]

usableRegs = filter (\x -> notElem x scratchRegs) [rax, rbx,
  r10, r11, r12, r13, r14, r15]

regCount = length usableRegs

-- State type
data AllocState = AllocState {
  freeRegs :: [Reg],
  activeIntervals :: [Interval],
  stackLocGen :: StackLocGen,
  stackSize :: Int,
  regUses :: Map Reg StorageType,
  tempRegs :: [Reg]
}
  deriving (Show)

data StorageType = InReg Reg
                 | InStack StackLoc
  deriving (Show, Eq)

isInStack :: StorageType -> Bool
isInStack (InStack _) = True
isInStack _ = False

-- Reg start end
type Interval = (Reg, Int, Int)

type AllocGen = StateT AllocState TempGen

emptyAllocState :: AllocState
emptyAllocState = AllocState {
  freeRegs = usableRegs,
  activeIntervals = [],
  stackLocGen = (-wordSize),
  stackSize = 0,
  regUses = Map.empty,
  tempRegs = scratchRegs
}

type StackLoc = Int
type StackLocGen = Int

newStackLoc :: AllocGen StackLoc
newStackLoc = do
  newLoc <- liftM stackLocGen get
  modify $ \st -> st {
    stackLocGen = newLoc - wordSize,
    stackSize = wordSize + stackSize st
  }
  return newLoc


addActiveInterval :: Interval -> AllocGen ()
addActiveInterval newVal = do
  actVals <- liftM activeIntervals get
  modify $ \st -> st {
    activeIntervals = List.sortBy compareInterval (newVal:actVals)
  }

setStorageType :: Reg -> StorageType -> AllocGen ()
setStorageType r sty = do
  regMap <- liftM regUses get
  modify $ \st -> st {
    regUses = Map.insert r sty regMap
  }

getStorageType :: Reg -> AllocGen StorageType
getStorageType r = do
  regMap <- liftM regUses get
  let (Just sty) = Map.lookup r regMap
  return sty

removeActiveInterval :: Interval -> AllocGen ()
removeActiveInterval rmVal = do
  actVals <- liftM activeIntervals get
  modify $ \st -> st {
    activeIntervals = List.sortBy compareInterval (List.delete rmVal actVals)
  }

alloc :: [DFInsn] -> TempGen [Insn]
alloc insnList = do
  (insnList', st) <- runAlloc insnList
  let size = stackSize st
      prologue = [
        Push (X64Op_I (IROp_R rbp)),
        Mov (X64Op_I (IROp_R rbp)) (X64Op_I (IROp_R rsp))] ++
        if size /= 0
          then [Sub (X64Op_I (IROp_R rsp)) (X64Op_I (IROp_I size))]
          else []
  return $ prologue ++ insnList'

runAlloc :: [DFInsn] -> TempGen ([Insn], AllocState)
runAlloc insnList = runStateT (alloc' insnList) emptyAllocState

alloc' :: [DFInsn] -> AllocGen [Insn]
alloc' insnList = do
  let lvMap = getLiveRange lvs
      lvs = map (\(DFInsn _ _ lv) -> lv) insnList
      sortedLvs = sortedLiveRange lvMap
  forM_ sortedLvs $ \iVal@(iReg, _, _) -> do
    expireOldIntervals iVal
    activeLen <- liftM (length . activeIntervals) get
    if activeLen == regCount
      then spillAtInterval iVal
      else do
        r <- allocReg
        addActiveInterval iVal
        setStorageType iReg (InReg r)
  liftM concat $ mapM materialize insnList

-- Replaces virtual registers in insn and adds spilling instructions if needed.
-- XXX Also does dead-code elimination when possible.
materialize :: DFInsn -> AllocGen [Insn]
materialize (DFInsn insn du lv) = do
  regMap <- liftM regUses get
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
          loadRegs = filter (isInStack . getItem) (getUse du)
          storeRegs = filter (isInStack . getItem) (getDef du)
          totalRegs = loadRegs `List.union` storeRegs
      --
      tempMap <- liftM Map.fromList $ forM totalRegs $ \r -> do
        let (InStack loc) = getItem r
        tempReg <- allocTempReg
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
                (Just (InReg mReg)) -> mReg
                _ -> error "RegAlloc.materialize: Internal Error"
      
      restoreTempRegs
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
      (InReg jMachReg) <- getStorageType jReg
      freeReg jMachReg
      expireOldIntervals' vs i

getLastInterval :: AllocGen Interval
getLastInterval = liftM (last . activeIntervals) get

spillAtInterval :: Interval -> AllocGen ()
spillAtInterval iVal@(iReg, iStart, iEnd) = do
  spill@(sReg, sStart, sEnd) <- getLastInterval
  if sEnd > iEnd
    then do
      (InReg r) <- getStorageType sReg
      loc <- newStackLoc
      setStorageType sReg (InStack loc)
      setStorageType iReg (InReg r)
      removeActiveInterval spill
      addActiveInterval iVal
    else do
      loc <- newStackLoc
      setStorageType iReg (InStack loc)
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

allocTempReg :: AllocGen Reg
allocTempReg = do
  (r:trs) <- liftM tempRegs get
  modify $ \st -> st {
    tempRegs = trs
  }
  return r

restoreTempRegs :: AllocGen ()
restoreTempRegs = do
  modify $ \st -> st {
    tempRegs = scratchRegs
  }

freeTempReg :: Reg -> AllocGen ()
freeTempReg r = do
  trs <- liftM freeRegs get
  modify $ \st -> st {
    tempRegs = (r:trs)
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
    addInterval lvMap (idx, Liveness lvIn lvOut) = newLvMap
      where
        newLvMap = foldr combine lvMap lvOut
        combine reg lvMap = case Map.lookup reg lvMap of
          (Just (frm, to)) -> Map.insert reg (frm, idx) lvMap
          Nothing -> Map.insert reg (idx, idx) lvMap

