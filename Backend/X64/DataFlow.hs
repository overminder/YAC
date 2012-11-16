module Backend.X64.DataFlow (
  DefUse(..),
  Liveness(..),
  DFInsn(..),
  runDefUse,
  runLiveness,
  iterLiveness
) where

import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Backend.IR.IROp
import Backend.X64.Insn
import Backend.X64.FlowGraph (FlowGraph)
import Backend.X64.Analyzable

-- DefUse and liveness information attached to an instruction
data DFInsn = DFInsn Insn DefUse Liveness
  deriving (Eq)

instance Show DFInsn where
  show (DFInsn i d l) = show i ++ " ;;      " ++ show d ++ " " ++ show l

instance GasSyntax DFInsn where
  gasShow (DFInsn i _ _) = gasShow i

data DefUse = DefUse {
  getDef :: [Reg],
  getUse :: [Reg]
}
  deriving (Eq)

instance Show DefUse where
  show du = "defs=" ++ show (getDef du) ++ ", uses=" ++ show (getUse du)

emptyDefUse = DefUse [] []

-- two sets of registers
data Liveness = Liveness {
  liveIn :: [Reg],
  liveOut :: [Reg]
}
  deriving (Eq)
instance Show Liveness where
  show lv = "liveIn=" ++ show (liveIn lv) ++ ",liveOut=" ++ show (liveOut lv)

emptyLiveness = Liveness [] []

runDefUse :: FlowGraph Insn -> FlowGraph DFInsn
runDefUse = fmap mkDefUse

mkDefUse :: Insn -> DFInsn
mkDefUse insn = DFInsn insn (mkDefUse' insn) emptyLiveness

mkDefUse' :: Insn -> DefUse
mkDefUse' insn = case insn of
  (Add dest src) -> getDefUse [dest] [dest, src]
  (Sub dest src) -> getDefUse [dest] [dest, src]
  (Sal dest src) -> getDefUse [dest] [dest, src]
  (Sar dest src) -> getDefUse [dest] [dest, src]
  (Or dest src)  -> getDefUse [dest] [dest, src]
  (And dest src) -> getDefUse [dest] [dest, src]
  (Call dest) -> getDefUse [] [dest]
  (Cmp lhs rhs) -> getDefUse [] [lhs, rhs]
  (Test lhs rhs) -> getDefUse [] [lhs, rhs]
  (Jmp dest) -> getDefUse [] [dest]
  (Lea dest src) -> getDefUse [dest] [src]
  (Mov dest src _) -> getDefUse [dest] [src]
  (Push src) -> getDefUse [] [src]
  (Pop dest) -> getDefUse [dest] []
  (PInsn InsertPrologue) -> emptyDefUse
  (PInsn InsertEpilogue) -> emptyDefUse
  (J _ _) -> emptyDefUse
  (BindLabel _) -> emptyDefUse
  Ret -> emptyDefUse
  --_ -> error $ "DataFlow.mkDefUse: undefined insn: " ++ show insn


mergeDefUse :: DefUse -> DefUse -> DefUse
mergeDefUse du0 du1 = DefUse {
  getDef = getDef du0 `List.union` getDef du1,
  getUse = getUse du0 `List.union` getUse du1
}

getDefUse :: [X64Op] -> [X64Op] -> DefUse
getDefUse defs uses = mergeDefUse du0 du1
  where
    --excludeMReg (DefUse xs ys) = DefUse (filter isVReg xs) (filter isVReg ys)

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

runLiveness :: FlowGraph DFInsn -> FlowGraph DFInsn
runLiveness = backwardAnalysis livenessAnalysis

iterLiveness :: FlowGraph DFInsn -> Int -> (FlowGraph DFInsn, Int)
iterLiveness g n
  | g' == g = (g, n)
  | otherwise = iterLiveness g' (n+1) 
  where
    g' = runLiveness g

{- Pseudo code for liveness analysis
     thisLv.liveIn = du.use `union` (thisLv.liveOut - du.def)
     thisLv.liveOut = nextLv.liveIn -}
livenessAnalysis :: DFInsn -> [DFInsn] -> DFInsn
livenessAnalysis i succs = mk i allLiveIns
  where
    mk :: DFInsn -> [Reg] -> DFInsn
    mk (DFInsn insns du _) nextIns
      = (DFInsn insns du (Liveness liveIn' liveOut'))
      where
        liveIn' = (getUse du) `List.union` (liveOut' List.\\ (getDef du))
        liveOut' = nextIns

    allLiveIns = foldr List.union [] (map getIn succs)
    getIn (DFInsn _ _ lv) = liveIn lv

