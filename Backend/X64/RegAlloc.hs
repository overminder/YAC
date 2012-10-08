module Backend.X64.RegAlloc where

import qualified Backend.IR.Operand as IROp

regList = map IROp.MachReg ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
numRegs = length regList

[rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]
  = regList


