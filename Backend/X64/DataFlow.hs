module Backend.X64.DataFlow (
  DefUse(..),
  mkDefUse,
  Liveness(..),
  DFInsn(..),
  runLiveness
) where

import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Backend.IR.IROp
import Backend.X64.Insn
import qualified Backend.X64.BasicBlock as BB
import qualified Backend.X64.FlowGraph as FG

-- DefUse and liveness information attached to an instruction
data DFInsn = DFInsn Insn DefUse Liveness
  deriving (Eq)

instance Show DFInsn where
  show (DFInsn i d l) = show i ++ " ;;      " ++ show d ++ " " ++ show l

data DefUse = DefUse {
  getDef :: [Reg],
  getUse :: [Reg]
}
  deriving (Eq)

instance Show DefUse where
  show du = "defs=" ++ show (getDef du) ++ ", uses=" ++ show (getUse du)

mergeDefUse :: DefUse -> DefUse -> DefUse
mergeDefUse du0 du1 = DefUse {
  getDef = getDef du0 `List.union` getDef du1,
  getUse = getUse du0 `List.union` getUse du1
}

emptyDefUse = DefUse [] []

mkDefUse :: Insn -> DFInsn
mkDefUse insn = DFInsn insn (mkDefUse' insn) emptyLiveness

mkDefUse' :: Insn -> DefUse
mkDefUse' insn = case insn of
  (Add dest src) -> getDefUse [dest] [dest, src]
  (Sub dest src) -> getDefUse [dest] [dest, src]
  (Cmp lhs rhs) -> getDefUse [] [lhs, rhs]
  (Lea dest src) -> getDefUse [dest] [src]
  (Mov dest src) -> getDefUse [dest] [src]
  (Push src) -> getDefUse [] [src]
  (Pop dest) -> getDefUse [dest] []
  _ -> emptyDefUse

getDefUse :: [X64Op] -> [X64Op] -> DefUse
getDefUse defs uses = excludeMReg $ mergeDefUse du0 du1
  where
    excludeMReg (DefUse xs ys) = DefUse (filter isVReg xs) (filter isVReg ys)

    du0 = foldr (mergeDefUse . fromDef) emptyDefUse defs
    du1 = foldr (mergeDefUse . fromUse) emptyDefUse uses

    fromDef :: X64Op -> DefUse
    fromDef (X64Op_I (IROp_R r)) = emptyDefUse{getDef=[r]}
    fromDef (X64Op_M (Address base index _ _)) =
      emptyDefUse{getUse=[base] ++ justToList index}
    fromDef _ = emptyDefUse
    fromUse :: X64Op -> DefUse
    fromUse (X64Op_I (IROp_R r)) = emptyDefUse{getUse=[r]}
    fromUse (X64Op_M (Address base index _ _)) =
      emptyDefUse{getUse=[base] ++ justToList index}
    fromUse _ = emptyDefUse

    isReg :: X64Op -> Bool
    isReg (X64Op_I (IROp_R _)) = True
    isReg _ = False

    unReg :: X64Op -> Reg
    unReg (X64Op_I (IROp_R r)) = r

    justToList (Just a) = [a]
    justToList Nothing = []

-- two sets of registers
data Liveness = Liveness {
  liveIn :: [Reg],
  liveOut :: [Reg]
}
  deriving (Eq)
instance Show Liveness where
  show lv = "liveIn=" ++ show (liveIn lv) ++ ",liveOut=" ++ show (liveOut lv)

emptyLiveness = Liveness [] []

type FlowGraph = FG.FlowGraph DFInsn
type FlowGraphGen = State FlowGraph

runLiveness :: FlowGraph -> FlowGraph
runLiveness = execState runLiveness'

-- Backward analysis
runLiveness' :: FlowGraphGen ()
runLiveness' = do
  bIdList <- liftM (reverse . FG.topologicallySortedBlockIds) get
  forM_ bIdList $ \bid -> do
    bbInsns <- getFullInsnList bid
    succInsns <- getSuccInsns bid
    let newBBInsn = mkLiveness bbInsns succInsns
    setFullInsnList bid newBBInsn

getFullInsnList :: BB.Id -> FlowGraphGen [DFInsn]
getFullInsnList bid = do
  bb <- liftM (FG.getBlock bid) get
  return $ BB.getFullInsnList bb

setFullInsnList :: BB.Id -> [DFInsn] -> FlowGraphGen ()
setFullInsnList bid insnList = do
  bb <- liftM (FG.getBlock bid) get
  let newBB = BB.setFullInsnList insnList bb
  modify $ FG.setBlock newBB

getSuccInsns :: BB.Id -> FlowGraphGen [DFInsn]
getSuccInsns bid = do
  succMap <- liftM FG.succs get
  let succBids = case Map.lookup bid succMap of
        (Just sbs) -> sbs
        Nothing -> []
  forM succBids $ \bid -> do
    bb <- liftM (FG.getBlock bid) get
    return $ BB.getFirstInsn bb

{- [initInsns...] [lastInsns] -> [liveness recalculated initInsns...]

   Pseudo Code:
     thisLv.liveIn = du.use `union` (thisLv.liveOut - du.def)
     thisLv.liveOut = nextLv.liveIn

   -OR-
     thisLv <- emptyLv
     forM du.use \v -> thisLv.liveIn.add v
     forM nextLv.liveIn \v -> thisLv.liveOut.add v
     forM thisLv.liveOut \v -> do 
       guard (v `notElem` du.def)
       thisLve.liveIn.add v
 -}
mkLiveness :: [DFInsn] -> [DFInsn] -> [DFInsn]
mkLiveness insnList lastInsns
  = concatCheck $ List.init $ foldr combine [lastInsns] insnList
  where
    concatCheck xs = if any (/=1) (map length xs)
      then error "mkLiveness: concatCheck failed"
      else concat xs

    combine :: DFInsn -> [[DFInsn]] -> [[DFInsn]]
    combine thisInsn nextInsnss@(nextInsns:_)
      = [mk' thisInsn $ allLiveIns nextInsns]:nextInsnss

    allLiveIns :: [DFInsn] -> [Reg]
    allLiveIns insns = foldr List.union [] (map getIn insns)
      where
        getIn (DFInsn _ _ lv) = liveIn lv

    mk' :: DFInsn -> [Reg] -> DFInsn
    mk' (DFInsn insns du _) nextIns
      = (DFInsn insns du (Liveness liveIn' liveOut'))
      where
        liveIn' = (getUse du) `List.union` (liveOut' List.\\ (getDef du))
        liveOut' = nextIns


