module Backend.X64.DataFlow (
  DefUse(..),
  getDefUse,
  Liveness(..),
  getLiveness
) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Backend.IR.IROp
import Backend.X64.Insn

data DefUse = DefUse {
  getDef :: [Reg],
  getUse :: [Reg]
}

mergeDefUse :: DefUse -> DefUse -> DefUse
mergeDefUse du0 du1 = DefUse {
  getDef = getDef du0 `List.union` getDef du1,
  getUse = getUse du0 `List.union` getUse du1
}

instance Show DefUse where
  show du = "defs=" ++ show (getDef du) ++ ", uses=" ++ show (getUse du)

emptyDefUse = DefUse [] []

getDefUse :: Insn -> DefUse
getDefUse (Add dest src) = makeDefUse [dest] [dest, src]
getDefUse (Sub dest src) = makeDefUse [dest] [dest, src]
getDefUse (Cmp lhs rhs) = makeDefUse [] [lhs, rhs]
getDefUse (Lea dest src) = makeDefUse [dest] [src]
getDefUse (Mov dest src) = makeDefUse [dest] [src]
getDefUse (Push src) = makeDefUse [] [src]
getDefUse (Pop dest) = makeDefUse [dest] []
--getDefUse Ret = makeDefUse [] [X64Op_I $ IROp_R rax] -- Hackish?
getDefUse _ = emptyDefUse

makeDefUse :: [X64Op] -> [X64Op] -> DefUse
makeDefUse defs uses = excludeMReg $ mergeDefUse du0 du1
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
instance Show Liveness where
  show lv = "liveIn=" ++ show (liveIn lv) ++ ",liveOut=" ++ show (liveOut lv)

emptyLiveness = Liveness [] []

{- Backward analysis
   thisLv.liveIn = du.use `union` (thisLv.liveOut - du.def)
   thisLv.liveOut = nextLv.liveIn

   thisLv <- emptyLv
   forM du.use \v -> thisLv.liveIn.add v
   forM nextLv.liveIn \v -> thisLv.liveOut.add v
   forM thisLv.liveOut \v -> do 
     guard (v `notElem` du.def)
     thisLve.liveIn.add v
 -}
getLiveness :: [DefUse] -> [Liveness]
getLiveness = List.init . (foldr combine [emptyLiveness])
  where
    combine :: DefUse -> [Liveness] -> [Liveness]
    combine du nextLvs@(nextLv:_) = (getLiveness' du nextLv):nextLvs
    getLiveness' :: DefUse -> Liveness -> Liveness
    getLiveness' du nextLv = Liveness liveIn' liveOut'
      where
        liveIn' = (getUse du) `List.union` (liveOut' List.\\ (getDef du))
        liveOut' = liveIn nextLv


