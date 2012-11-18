module Backend.X64.GraphBuilder (
  buildGraph
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
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
  gbDefConns :: [(BB.Id, Imm)],
  gbLabelMap :: Map Imm BB.Id
}
  deriving (Show)

empty :: GraphBuilder
empty = GraphBuilder {
  gbGraph = FG.empty,
  gbCurrBlock = BB.empty,
  gbDefConns = [],
  gbLabelMap = Map.empty
}

type GraphGen = StateT GraphBuilder F.FrameGen

getCurrBlock :: GraphGen BasicBlock
getCurrBlock = liftM gbCurrBlock get

getCurrBlockId :: GraphGen BB.Id
getCurrBlockId = liftM (BB.bId . gbCurrBlock) get

setCurrBlockId :: BB.Id -> GraphGen ()
setCurrBlockId newId = do
  modify $ \st -> st {
    gbCurrBlock = (gbCurrBlock st) { BB.bId = newId }
  }

buildGraph :: [Insn] -> F.FrameGen FlowGraph
buildGraph insnList = liftM gbGraph (execStateT (buildGraph' insnList) empty)

buildGraph' :: [Insn] -> GraphGen ()
buildGraph' insnList = do
  entryId <- createBlock
  setEntry entryId
  mapM_ addOneInsn insnList
  resolveDefConns

pass :: Monad m => m ()
pass = return ()

addOneInsn :: Insn -> GraphGen ()
addOneInsn insn
  -- for `Jmp', we have several cases
  -- 1. non-jump
  --    Jmp .Label <-
  --
  -- 2. Jmp XX
  --    Jmp .Label <-
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
  --    Label: <-
  -- For this case, there's no need to create new block
  --
  -- 2. mov XXX
  --    Label: <-
  -- For this case, we should create new block
  --
  -- 3. Label:
  --    Label: <-
  -- For this case, we can just merge the two labels together
  | isBranchTarget insn = do
      let (BindLabel (X64Op_I (IROp_I label))) = insn
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
    case Map.lookup label labelMap of
      (Just toB) -> connectBlock fromB toB
      Nothing -> error $ "GraphBuilder.resolveDefConns: no block for label "
                         ++ show label

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

addDeferredConnection :: BB.Id -> Imm -> GraphGen ()
addDeferredConnection bid label = modify $ \st -> st {
  gbDefConns = (bid, label):(gbDefConns st)
}

addLabelMapping :: Imm -> GraphGen ()
addLabelMapping label = do
  bid <- getCurrBlockId
  modify $ \st -> st {
    gbLabelMap = Map.insert label bid (gbLabelMap st),
    gbCurrBlock = BB.addLabel (BindLabel $ X64Op_I $ IROp_I label)
                              (gbCurrBlock st)
  }

setEntry :: BB.Id -> GraphGen ()
setEntry bid = do
  modify $ \st -> st {
    gbGraph = FG.setEntry bid (gbGraph st)
  }

