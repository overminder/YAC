module Backend.X64.RegAlloc where

import Control.Monad.State

import qualified Backend.IR.Operand as IROp

-- X64 reg spec
allRegs = map IROp.MachReg ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi",
    "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

[rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]
  = allRegs

usableRegs = [rax, rbx, rcx, rdx, rdi, rsi,
    r8, r9, r10, r11, r12, r13, r14, r15]

regCount = length usableRegs

-- State type
data AllocState = AllocState {
  freeRegs :: [IROp.Reg],
  activeIntervals :: [RegInterval],
  insnId :: Int
}

emptyAllocState :: AllocState
emptyAllocState = AllocState {
  freeRegs = usableRegs,
  activeIntervals = [],
  insnId = 0
}

type Allocator = State Allocator

execAlloc :: [Insn] -> [Liveness] -> [Insn]
execAlloc ins lvs = execState (alloc ins lvs) emptyAllocState

-- PseudoReg start end
data RegInterval = RegInterval IROp.Reg Int Int

-- PseudoReg MachReg
data ActiveReg = ActiveReg IROp.Reg IROp.Reg

-- entry point
alloc :: [Insn] -> [Liveness] -> Allocator [Insn]

-- result is sorted in increasing start point order
getIntervals :: [Liveness] -> [RegInterval]
--getIntervals 

expireOldIntervals :: [ActiveReg], [IROp.Regs]

