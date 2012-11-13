module Main where

import System
import Control.Monad
import qualified Data.List as List

import Backend.IR.IROp (IROp (..), Reg (..), getReg)
import Backend.IR.Temp
import qualified Backend.IR.Tree as IRTree

import Backend.X64.Insn
import Backend.X64.Munch
import Backend.X64.DataFlow
import Backend.X64.FlowGraph
import Backend.X64.GraphBuilder
import Backend.X64.RegAlloc

import Frontend.ObjModel
import Frontend.Parser
--import Frontend.Rewrite
import qualified Frontend.IRGen as IRGen

prettifyDataFlow :: [Insn] -> String
prettifyDataFlow insnList = List.intercalate "\n" outputStrList
  where
    duList = map getDefUse insnList
    lvList = getLiveness duList
    outputStrList = map prettify (List.zip3 insnList duList lvList)
    prettify (insn,du,lv) = leftAlign (show insn) 30 ++ ";; " ++ 
      show du ++ "| " ++ show lv
    leftAlign s howMany =
      if length s < howMany
        then leftAlign (s ++ " ") howMany
        else s

prettifyInsnOnly :: [Insn] -> String
prettifyInsnOnly insnList = List.intercalate "\n" (map show insnList)

visualize0 :: Cell -> IO ()
visualize0 prog = do
  let (tree, insns, regAllocInsns, allocState) = runTempGen $ do 
        tree <- IRGen.gen prog
        insns <- munch tree
        let lvs = getLiveness (map getDefUse insns)
        (regAllocInsns, allocState) <- runAlloc insns lvs
        return (tree, insns, regAllocInsns, allocState)
  putStrLn $ "\t.ir-tree\n" ++ show tree
  putStrLn $ "\t.insn\n" ++ prettifyDataFlow insns
  putStrLn $ "\t.live-range\n" ++ show (
    getLiveRange (getLiveness (map getDefUse insns)))
  putStrLn $ "\t.alloc-state\n" ++ show allocState
  putStrLn $ "\t.regalloc-insn\n" ++ prettifyInsnOnly regAllocInsns

visualize1 :: Cell -> IO ()
visualize1 prog = do
  let (tree, insns, builder) = runTempGen $ do 
        tree <- IRGen.gen prog
        insns <- munch tree
        builder <- buildGraph insns
        return (tree, insns, builder)
  putStrLn $ "\t.ir-tree\n" ++ show tree
  putStrLn $ "\t.insn\n" ++ prettifyInsnOnly insns
  putStrLn $ "\t.graph-builder\n" ++ show builder

main = do
  input <- getContents
  let (List c) = readProg input
  putStrLn $ "\t.parse-result\n" ++ show (List c)
  case c of
    [Symbol "parse-success", prog] -> visualize1 prog
    [Symbol "parse-error", what] -> putStrLn $ show what

