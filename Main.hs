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
import Frontend.IRGen

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
  let c = readCell input
  putStrLn $ show c
  case pairToList c of
    ([Symbol "parse-success", prog], _) -> do
      let tree = gen prog
      putStrLn $ show tree
      let insns = munch tree
      putStrLn $ prettifyDataFlow insns
    ([Symbol "parse-error", what], _) -> putStrLn $ show what

