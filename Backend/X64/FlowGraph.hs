module Backend.X64.FlowGraph (
  FlowGraph(..),
  empty,
  setEntry,
  connect,
  addSucc,
  addPred,
  addBlock,
  getBlock
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Backend.X64.BasicBlock (BasicBlock)
import qualified Backend.X64.BasicBlock as BB

data FlowGraph = FlowGraph {
  fgEntry :: BB.Id,
  fgBlocks :: Map BB.Id BasicBlock,
  fgPreds :: Map BB.Id [BB.Id],
  fgSuccs :: Map BB.Id [BB.Id]
}
  deriving (Show)

empty = FlowGraph {
  fgEntry = (-1),
  fgBlocks = Map.empty,
  fgPreds = Map.empty,
  fgSuccs = Map.empty
}

setEntry :: BB.Id -> FlowGraph -> FlowGraph
setEntry bid fg = fg {
  fgEntry = bid
}

connect :: BB.Id -> BB.Id -> FlowGraph -> FlowGraph
connect f t g =
  addSucc f t (addPred f t g)

addSucc :: BB.Id -> BB.Id -> FlowGraph -> FlowGraph
addSucc f t g = newG
  where
    succMap = fgSuccs g
    newG = g {
      fgSuccs = Map.alter alter f succMap
    }
    alter v = case v of
      (Just xs) -> Just (t:xs)
      Nothing -> Just [t]

addPred :: BB.Id -> BB.Id -> FlowGraph -> FlowGraph
addPred f t g = newG
  where
    predMap = fgPreds g
    newG = g {
      fgPreds = Map.alter alter t predMap
    }
    alter v = case v of
      (Just xs) -> Just (f:xs)
      Nothing -> Just [f]

addBlock :: BasicBlock -> FlowGraph -> FlowGraph
addBlock bb g = g {
  fgBlocks = Map.insert (BB.bId bb) bb (fgBlocks g)
}

getBlock :: BB.Id -> FlowGraph -> BasicBlock
getBlock bid g = (fgBlocks g) Map.! bid

