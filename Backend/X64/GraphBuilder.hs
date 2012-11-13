module Backend.X64.GraphBuilder (
  buildGraph
) where

import Control.Monad.State

import Backend.IR.Temp
import Backend.X64.Insn
import Backend.X64.BasicBlock (BasicBlock)
import qualified Backend.X64.BasicBlock as BB
import Backend.X64.FlowGraph (FlowGraph)
import qualified Backend.X64.FlowGraph as FG

data GraphBuilder = GraphBuilder {
  gbGraph :: FlowGraph,
  gbCurrBlock :: BasicBlock,
  gbCurrBlockId :: BB.Id
}
  deriving (Show)

emptyGraphBuilder = GraphBuilder {
  gbGraph = FG.empty,
  gbCurrBlock = BB.empty,
  gbCurrBlockId = (-1)
}

type GraphGen = StateT GraphBuilder TempGen

buildGraph :: [Insn] -> GraphGen ()
buildGraph insnList = do
  newBlockId <- lift nextTemp
  modify $ \st -> st {
    gbCurrBlockId = newBlockId
  }
  mapM_ addOneInsn insnList

addOneInsn :: Insn -> GraphGen ()
addOneInsn insn
  | isBranchInsn insn = do
      currBlock <- liftM gbCurrBlock get
      let modifiedBlock = BB.setCtrlInsn insn currBlock
      return () -- TODO

  | otherwise = addToCurrBlock insn

addToCurrBlock :: Insn -> GraphGen ()
addToCurrBlock insn = do
  currBlock <- liftM gbCurrBlock get
  modify $ \st -> st {
    gbCurrBlock = BB.addInsn insn currBlock
  }


