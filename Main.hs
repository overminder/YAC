import Control.Monad
import Control.Monad.Writer
import qualified Data.List as List

import Util.Temp

import Backend.X64.Munch
import Backend.X64.DataFlow
import Backend.X64.FlowGraph
import Backend.X64.GraphBuilder
import Backend.X64.RegAlloc
import qualified Backend.X64.Frame as F
import Backend.X64.Peephole

import Frontend.ObjModel
import Frontend.Parser
import qualified Frontend.IRGen as IRGen

showMany :: Show a => [a] -> String
showMany insnList = List.intercalate "\n" (map show insnList)

visualize1 :: Cell -> IO ()
visualize1 prog = do
  let output = runTempGen $ do 
        toplevelDefs <- IRGen.gen prog
        forM toplevelDefs $ \d -> do
          case d of
            (IRGen.FuncDef name formals tree) -> F.runFrameGen $ do
              F.setFuncName name
              F.setFuncArgs formals
              rawInsns <- munch tree
              rawInsns1 <- runPeephole rawInsns
              graph <- liftM runDefUse $ buildGraph rawInsns1
              let (graph1, _) = iterLiveness graph 0
                  flowInsns = toTrace graph1
              allocInsns <- alloc flowInsns
              allocInsns1 <- F.insertProAndEpilogue allocInsns
              -- second graph, to get liveness around call sites
              graph2 <- liftM runDefUse $ buildGraph allocInsns1
              let (graph3, _) = iterLiveness graph2 0
                  flowInsns' = toTrace graph3
              insns1 <- F.insertCallerSave flowInsns'
              insns2 <- F.patchCalleeMovArg insns1
              finalOutput <- F.formatOutput insns2
              --let outputs = [show tree,
              --               showMany rawInsns,
              --               showMany flowInsns,
              --               finalOutput]
              --return $ List.intercalate "\n------\n" outputs
              return finalOutput
            (IRGen.QuadDef name maybeVal) -> execWriterT $ do
              let writeLn s = tell s >> tell "\n"
              writeLn ".data"
              writeLn ".align 8"
              writeLn $ name ++ ":"
              writeLn $ ".quad " ++ case maybeVal of
                (Just i) -> show i
                Nothing -> "0"
            (IRGen.StringDef name val) -> execWriterT $ do
              let writeLn s = tell s >> tell "\n"
              writeLn ".data"
              writeLn ".align 8"
              writeLn $ name ++ ":"
              writeLn $ ".string \"" ++ val ++ "\""
  mapM_ putStrLn output

main :: IO ()
main = do
  input <- getContents
  let (List c) = readProg input
  --putStrLn $ "\t.parse-result\n" ++ show (List c)
  case c of
    [Symbol "parse-success", prog] -> visualize1 prog
    [Symbol "parse-error", what] -> putStrLn $ show what
    _ -> error "not reached"

