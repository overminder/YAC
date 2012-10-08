module Main where

import System
import qualified Data.List as List
import Control.Monad

import Backend.IR.Operand (Operand (..), Reg (..), getReg)
import qualified Backend.IR.Tree as IRTree
import Backend.X64.Insn
import Backend.X64.Munch
import qualified Backend.X64.RegAlloc as RegAlloc
import qualified Backend.X64.DataFlow as DataFlow

import Frontend.ObjModel
import Frontend.Parser (readCell)

[pr0, pr1, pr2, pr3] = map (RegOperand . PseudoReg) [0, 1, 2, 3]

irInsn = IRTree.Seq
  (IRTree.Move (IRTree.Leaf pr0) (IRTree.Leaf (ImmOperand 1)))
  (IRTree.Seq
    (IRTree.Move (IRTree.Leaf pr1) (IRTree.Leaf (ImmOperand 2)))
    (IRTree.Seq
      (IRTree.Move (IRTree.Leaf pr2) (IRTree.Add (IRTree.Leaf pr0)
                                                 (IRTree.Leaf pr1)))
      (IRTree.Move (IRTree.Leaf pr3) (IRTree.Add (IRTree.Leaf pr0)
                                                 (IRTree.Leaf (ImmOperand 3))))))

x64Insn :: [Insn]
x64Insn = munch irInsn

liveness :: [DataFlow.DefUse] -> [DataFlow.Liveness]
liveness = List.init . (foldr f [DataFlow.emptyLiveness])
  where
    f du nextLvs@(nextLv:_) = (DataFlow.getLiveness du nextLv):nextLvs


prettifyDataFlow :: [Insn] -> String
prettifyDataFlow insnList = List.intercalate "\n" outputStrList
  where
    duList = map DataFlow.getDefUse insnList
    lvList = liveness duList
    outputStrList = map prettify (List.zip3 insnList duList lvList)
    prettify (insn,du,lv) = leftAlign (show insn) 30 ++ ";; " ++ 
      show du ++ "| " ++ show lv
    leftAlign s howMany =
      if length s < howMany
        then leftAlign (s ++ " ") howMany
        else s

main = do
  input <- liftM (!!0) getArgs
  putStrLn $ readCell input
