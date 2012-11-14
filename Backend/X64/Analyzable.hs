module Backend.X64.Analyzable (
  Analyzable(..)
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Backend.X64.FlowGraph as FG
import qualified Backend.X64.BasicBlock as BB

-- [u | u <- Pred(i)] -> i -> i'
type ForwardAnalyzer a = [a] -> a -> a

-- i -> [u | u <- Succ(i)] -> i'
type BackwardAnalyzer a = a -> [a] -> a

class Analyzable k where
  forwardAnalysis :: ForwardAnalyzer a -> k a -> k a
  backwardAnalysis :: BackwardAnalyzer a -> k a -> k a

instance Analyzable FG.FlowGraph where
  forwardAnalysis g f = undefined
  backwardAnalysis = execState . runBackward

type FlowGraphGen a = State (FG.FlowGraph a)

runBackward :: BackwardAnalyzer a -> FlowGraphGen a ()
runBackward f = do
  revBidList <- liftM (reverse . FG.topologicallySortedBlockIds) get
  forM_ revBidList $ \bid -> do
    bbInsns <- getFullInsnList bid
    succInsns <- getSuccInsns bid
    let newBBInsn = runBackwardOnBasicBlock f bbInsns succInsns
    setFullInsnList bid newBBInsn

runBackwardOnBasicBlock :: BackwardAnalyzer a -> [a] -> [a] -> [a]
runBackwardOnBasicBlock f insnList lastInsns
  = concat $ List.init $ foldr combine [lastInsns] insnList
  where
    combine thisInsn nextInsnss@(nextInsns:_)
      = [f thisInsn nextInsns]:nextInsnss

getFullInsnList :: BB.Id -> FlowGraphGen a [a]
getFullInsnList bid = do
  bb <- liftM (FG.getBlock bid) get
  return $ BB.getFullInsnList bb

setFullInsnList :: BB.Id -> [a] -> FlowGraphGen a ()
setFullInsnList bid insnList = do
  bb <- liftM (FG.getBlock bid) get
  let newBB = BB.setFullInsnList insnList bb
  modify $ FG.putBlock newBB

getSuccInsns :: BB.Id -> FlowGraphGen a [a]
getSuccInsns bid = do
  succMap <- liftM FG.succs get
  let succBids = case Map.lookup bid succMap of
        (Just sbs) -> sbs
        Nothing -> []
  forM succBids $ \bid -> do
    bb <- liftM (FG.getBlock bid) get
    return $ BB.getFirstInsn bb

