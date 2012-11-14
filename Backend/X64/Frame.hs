module Backend.X64.Frame (
  Frame(..),
  FrameGen,
  runFrameGen,
  insertProAndEpilogue,
  formatOutput,

  nextTemp,
  newStackLoc,
  setStorageType,
  getStorageType,
  allocReg,
  freeReg,
  allocTempReg,
  restoreTempRegs,

  StorageType(..),
  isInStack,

  wordSize,
  argRegs,
  regCount,
) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.IR.IROp
import qualified Backend.IR.Temp as Temp
import Backend.X64.Insn

data Frame = Frame {
  name :: String,
  vRegUses :: Map Reg StorageType,
  stackLocGen :: Int,
  frameSize :: Int,
  mRegUses :: [Reg],
  freeRegs :: [Reg],
  tempRegs :: [Reg]
}

type FrameGen = StateT Frame Temp.TempGen

data StorageType = InReg Reg
                 | InStack StackLoc
  deriving (Show, Eq)

isInStack :: StorageType -> Bool
isInStack (InStack _) = True
isInStack _ = False

type StackLoc = Int
type StackLocGen = Int

-- Defs

scratchRegs = [r13, r14, r15]

usableRegs = filter (\x -> notElem x scratchRegs) [rax, rbx,
  r10, r11, r12, r13, r14, r15]

regCount = length usableRegs

wordSize :: Int
wordSize = 8

argRegs :: [X64Op]
argRegs = map (X64Op_I . IROp_R) [rdi, rsi, rdx, rcx, r8, r9] ++ stackArgGen
  where
    stackArgGen = map (X64Op_M . nthStackArg) [0..]
    nthStackArg n = Address rsp Nothing Scale1 (wordSize * n)

empty :: Frame
empty = Frame {
  name = "test",
  vRegUses = Map.empty,
  stackLocGen = (-wordSize),
  frameSize = 0,
  mRegUses = [],
  freeRegs = usableRegs,
  tempRegs = scratchRegs
}

nextTemp :: FrameGen Int
nextTemp = lift Temp.nextTemp

useReg :: Reg -> FrameGen ()
useReg r = modify $ \st -> st {
  mRegUses = [r] `List.union` mRegUses st
}

runFrameGen :: FrameGen a -> Temp.TempGen a
runFrameGen m = evalStateT m empty

newStackLoc :: FrameGen StackLoc
newStackLoc = do
  newLoc <- liftM stackLocGen get
  modify $ \st -> st {
    stackLocGen = newLoc - wordSize,
    frameSize = wordSize + frameSize st
  }
  return newLoc

setStorageType :: Reg -> StorageType -> FrameGen ()
setStorageType r sty = do
  regMap <- liftM vRegUses get
  modify $ \st -> st {
    vRegUses = Map.insert r sty regMap
  }

getStorageType :: Reg -> FrameGen StorageType
getStorageType r = do
  regMap <- liftM vRegUses get
  let (Just sty) = Map.lookup r regMap
  return sty

allocReg :: FrameGen Reg
allocReg = do
  (r:frs) <- liftM freeRegs get
  modify $ \st -> st {
    freeRegs = frs
  }
  useReg r
  return r

freeReg :: Reg -> FrameGen ()
freeReg r = do
  frs <- liftM freeRegs get
  modify $ \st -> st {
    freeRegs = (r:frs)
  }

allocTempReg :: FrameGen Reg
allocTempReg = do
  (r:trs) <- liftM tempRegs get
  modify $ \st -> st {
    tempRegs = trs
  }
  return r

restoreTempRegs :: FrameGen ()
restoreTempRegs = do
  modify $ \st -> st {
    tempRegs = scratchRegs
  }

insertProAndEpilogue :: [Insn] -> FrameGen [Insn]
insertProAndEpilogue insnList = do
  liftM concat $ forM insnList $ \insn -> do
    case insn of
      (PInsn InsertPrologue) -> do
        spaceNeeded <- liftM frameSize get
        return $ [Push (X64Op_I (IROp_R rbp)),
                  Mov (X64Op_I (IROp_R rbp)) (X64Op_I (IROp_R rsp))] ++
                  if spaceNeeded /= 0
                    then [Sub (X64Op_I (IROp_R rsp))
                              (X64Op_I (IROp_I spaceNeeded))]
                    else []
      (PInsn InsertEpilogue) ->
        return [Mov (X64Op_I (IROp_R rsp)) (X64Op_I (IROp_R rbp)),
                Pop (X64Op_I (IROp_R rbp))]
      _ -> return [insn]

formatOutput :: [Insn] -> FrameGen String
formatOutput insnList =
  let writeLn s = tell s >> tell "\n"
   in execWriterT $ do
        funcName <- lift $ liftM name get
        writeLn $ ".global " ++ funcName
        writeLn $ funcName ++ ":"
        forM_ insnList $ \insn -> do
          writeLn $ gasShow insn

