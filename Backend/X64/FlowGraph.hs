module Backend.X64.FlowGraph (
  FlowGraph(..),
  empty,
  setEntry,
  connect,
  addSucc,
  addPred,
  getBlock,
  putBlock,
  topologicallySortedBlockIds,
  toTrace
) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.X64.BasicBlock (BasicBlock)
import qualified Backend.X64.BasicBlock as BB

data FlowGraph a = FlowGraph {
  entry :: BB.Id,
  blocks :: Map BB.Id (BasicBlock a),
  preds :: Map BB.Id [BB.Id],
  succs :: Map BB.Id [BB.Id]
}
  deriving (Show, Eq)

instance Functor FlowGraph where
  fmap f g = g {
    blocks = Map.map (fmap f) (blocks g)
  }

empty = FlowGraph {
  entry = (-1),
  blocks = Map.empty,
  preds = Map.empty,
  succs = Map.empty
}

setEntry :: BB.Id -> FlowGraph a -> FlowGraph a
setEntry bid fg = fg {
  entry = bid
}

connect :: BB.Id -> BB.Id -> FlowGraph a -> FlowGraph a
connect f t g =
  addSucc f t (addPred f t g)

addSucc :: BB.Id -> BB.Id -> FlowGraph a -> FlowGraph a
addSucc f t g = newG
  where
    succMap = succs g
    newG = g {
      succs = Map.alter alter f succMap
    }
    alter v = case v of
      (Just xs) -> Just (t:xs)
      Nothing -> Just [t]

addPred :: BB.Id -> BB.Id -> FlowGraph a -> FlowGraph a
addPred f t g = newG
  where
    predMap = preds g
    newG = g {
      preds = Map.alter alter t predMap
    }
    alter v = case v of
      (Just xs) -> Just (f:xs)
      Nothing -> Just [f]

getBlock :: BB.Id -> FlowGraph a -> BasicBlock a
getBlock bid g = bb
  where
    (Just bb) = Map.lookup bid $ blocks g

putBlock :: BasicBlock a -> FlowGraph a -> FlowGraph a
putBlock bb g = g {
  blocks = Map.insert (BB.bId bb) bb (blocks g)
}

-- XXX hackish. Maybe it is okay for now...
topologicallySortedBlockIds :: FlowGraph a -> [BB.Id]
topologicallySortedBlockIds = List.sort . Map.keys . blocks

toTrace :: FlowGraph a -> [a]
toTrace g = concatMap BB.toTrace (map (\bid -> getBlock bid g) bids)
  where
    bids = topologicallySortedBlockIds g

