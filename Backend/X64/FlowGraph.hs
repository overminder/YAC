module Backend.X64.FlowGraph (
  FlowGraph(..),
  empty
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Backend.X64.BasicBlock (BasicBlock)
import qualified Backend.X64.BasicBlock as BB

data FlowGraph = FlowGraph {
  fgEntry :: BB.Id,
  fgBlocks :: Map BB.Id BasicBlock
}
  deriving (Show)

empty = FlowGraph {
  fgEntry = (-1),
  fgBlocks = Map.empty
}

