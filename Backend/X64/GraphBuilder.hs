module Backend.X64.GraphBuilder (
  buildGraph
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.X64.Insn
import qualified Backend.X64.BasicBlock as BB
import qualified Backend.X64.FlowGraph as FG
import qualified Backend.X64.Frame as F

-- Specialized version for graph building
type BasicBlock = BB.BasicBlock Insn
type FlowGraph = FG.FlowGraph Insn

data GraphBuilder = GraphBuilder {
  gbGraph :: FlowGraph,
  gbCurrBlock :: BasicBlock,
  gbDefConns :: [(BB.Id, Label)],
  gbLabelMap :: Map Label BB.Id
}
  deriving (Show)

emptyGraphBuilder = GraphBuilder {
  gbGraph = FG.empty,
  gbCurrBlock = BB.empty,
  gbDefConns = [],
  gbLabelMap = Map.empty
}

type GraphGen = StateT GraphBuilder F.FrameGen

getGraph :: GraphGen FlowGraph
getGraph = liftM gbGraph get

getCurrBlock :: GraphGen BasicBlock
getCurrBlock = liftM gbCurrBlock get

getCurrBlockId :: GraphGen BB.Id
getCurrBlockId = liftM (BB.bId . gbCurrBlock) get

setCurrBlockId :: BB.Id -> GraphGen ()
setCurrBlockId newId = do
  bb <- getCurrBlock
  modify $ \st -> st {
    gbCurrBlock = (gbCurrBlock st) { BB.bId = newId }
  }

buildGraph :: [Insn] -> F.FrameGen FlowGraph
buildGraph insnList = liftM gbGraph (execStateT (buildGraph' insnList) emptyGraphBuilder)

buildGraph' :: [Insn] -> GraphGen ()
buildGraph' insnList = do
  entryId <- createBlock
  setEntry entryId
  mapM_ addOneInsn insnList
  resolveDefConns

pass = return ()

addOneInsn :: Insn -> GraphGen ()
addOneInsn insn
  | isBranchInsn insn = do
      currBlock <- getCurrBlock
      modify $ \st -> st {
        gbCurrBlock = BB.setCtrlInsn insn currBlock
      }
      oldBlockId <- getCurrBlockId
      finishCurrBlock
      newBlockId <- createBlock
      if mightFallThrough insn
        then connectBlock oldBlockId newBlockId
        else pass
      case (getBranchTarget insn) of
        (Just label) -> addDeferredConnection oldBlockId label
        Nothing -> pass

  -- for `Label', we have three cases:
  -- 1. jmp/jz XXX
  --    Label: 
  -- For this case, there's no need to create new block
  --
  -- 2. mov XXX
  --    Label:
  -- For this case, we should create new block
  --
  -- 3. Label:
  --    Label: <-
  -- For this case, we can just merge the two labels together
  | isBranchTarget insn = do
      let (BindLabel label) = insn
      currBlock <- getCurrBlock
      if BB.hasNoInsn currBlock
        then pass -- case 1 and 3
        else do -- case 2
          oldBlockId <- getCurrBlockId
          finishCurrBlock
          newBlockId <- createBlock
          connectBlock oldBlockId newBlockId
      addLabelMapping label

  | otherwise = addToCurrBlock insn

resolveDefConns :: GraphGen ()
resolveDefConns = do
  defConns <- liftM gbDefConns get
  labelMap <- liftM gbLabelMap get
  forM_ defConns $ \(fromB, label) -> do
    let (Just toB) = Map.lookup label labelMap
    connectBlock fromB toB

addToCurrBlock :: Insn -> GraphGen ()
addToCurrBlock insn = do
  currBlock <- getCurrBlock
  modify $ \st -> st {
    gbCurrBlock = BB.addInsn insn currBlock
  }

finishCurrBlock :: GraphGen ()
finishCurrBlock = do
  modify $ \st -> st {
    gbGraph = FG.putBlock (gbCurrBlock st) (gbGraph st),
    gbCurrBlock = BB.empty
  }

createBlock :: GraphGen BB.Id
createBlock = do
  newId <- lift F.nextTemp
  modify $ \st -> st {
    gbCurrBlock = BB.empty
  }
  setCurrBlockId newId
  return newId

connectBlock :: BB.Id -> BB.Id -> GraphGen ()
connectBlock f t = do
  modify $ \st -> st {
    gbGraph = FG.connect f t (gbGraph st)
  }

addDeferredConnection :: BB.Id -> Label -> GraphGen ()
addDeferredConnection bid label = modify $ \st -> st {
  gbDefConns = (bid, label):(gbDefConns st)
}

addLabelMapping :: Label -> GraphGen ()
addLabelMapping label = do
  bid <- getCurrBlockId
  modify $ \st -> st {
    gbLabelMap = Map.insert label bid (gbLabelMap st),
    gbCurrBlock = BB.addLabel (BindLabel label) (gbCurrBlock st)
  }

setEntry :: BB.Id -> GraphGen ()
setEntry bid = do
  modify $ \st -> st {
    gbGraph = FG.setEntry bid (gbGraph st)
  }

