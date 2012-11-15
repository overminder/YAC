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
import Backend.X64.Frame

import Frontend.ObjModel
import Frontend.Parser
--import Frontend.Rewrite
import qualified Frontend.IRGen as IRGen

prettifyInsnOnly :: Show a => [a] -> String
prettifyInsnOnly insnList = List.intercalate "\n" (map show insnList)

visualize1 :: Cell -> IO ()
visualize1 prog = do
  let output = runTempGen $ do 
        toplevelDefs <- IRGen.gen prog
        forM toplevelDefs $ \d -> do
          case d of
            (IRGen.FuncDef name formals tree) -> runFrameGen $ do
              setFuncName name
              setFuncArgs formals
              rawInsns <- munch tree
              graph <- liftM runDefUse $ buildGraph rawInsns
              let (graph', niter) = iterLiveness graph 0
                  flowInsns = toTrace graph'
              allocInsns <- alloc flowInsns
              allocInsns <- insertProAndEpilogue allocInsns
              -- second graph, to get liveness around call sites
              graph <- liftM runDefUse $ buildGraph allocInsns
              let (graph', niter) = iterLiveness graph 0
                  flowInsns' = toTrace graph'
              insns' <- insertCallerSave flowInsns'
              insns' <- patchCalleeMovArg insns'
              formatOutput insns'
            _ -> error $ "Toplevel form not supported: " ++ show d
  mapM_ putStrLn output

main = do
  input <- getContents
  let (List c) = readProg input
  --putStrLn $ "\t.parse-result\n" ++ show (List c)
  case c of
    [Symbol "parse-success", prog] -> visualize1 prog
    [Symbol "parse-error", what] -> putStrLn $ show what

