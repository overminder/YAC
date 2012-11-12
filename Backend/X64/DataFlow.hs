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
getDefUse _ = emptyDefUse

makeDefUse :: [X64Op] -> [X64Op] -> DefUse
makeDefUse defs uses = mergeDefUse du0 du1
  where
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

-- actually it's the live var when entering the insn
data Liveness = Liveness {
  getLiveVars :: [Reg]
}
instance Show Liveness where
  show lv = "live=" ++ show (getLiveVars lv)

emptyLiveness = Liveness []

{- Backward analysis

   thisLv <- emptyLv
   for any given v in du.def, du.use and nextLv
     if v in du.use
       thisLv.add v
     else
       if v in nextLv
         if v in du.def
           pass
         else
           thisLv.add v -- pass throu
 -}
getLiveness :: [DefUse] -> [Liveness]
getLiveness = List.init . (foldr combine [emptyLiveness])
  where
    combine :: DefUse -> [Liveness] -> [Liveness]
    combine du nextLvs@(nextLv:_) = (getLiveness' du nextLv):nextLvs
    getLiveness' :: DefUse -> Liveness -> Liveness
    getLiveness' du nextLv = 
      Liveness $ List.union (foldr checkDef [] (getLiveVars nextLv)) (getUse du)
      where
        checkDef :: Reg -> [Reg] -> [Reg]
        checkDef x =
          if x `elem` (getDef du)
            then id
            else (x:)


