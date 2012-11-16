module Backend.X64.Peephole (
  runPeephole
) where

-- Peephole optimization is performed after munch and before regalloc.
-- This will be mostly useless if we can apply data flow optimizations.

import Control.Monad

import Backend.IR.IROp
import Backend.X64.Insn
import qualified Backend.X64.Frame as F

runPeephole :: [Insn] -> F.FrameGen [Insn]
runPeephole insnList = case insnList of
  -- mov r0, i; mov r1 r0; => mov r0, i; mov r1, i;
  (insn1@(Mov dest@(X64Op_I (IROp_R _)) src@(X64Op_I (IROp_I _)) _):
   insn2@(Mov dest2@(X64Op_I (IROp_R _)) src2@(X64Op_I (IROp_R _)) ty2):rest) ->
     if dest == src2
       then do
         let newInsn = Mov dest2 src ty2
         runPeephole (insn1:newInsn:rest)
       else do
         rest' <- runPeephole (insn2:rest)
         return (insn1:rest')

  -- mov r0, r1; mov r2 r0; => mov r0, r1; mov r2, 1;
  (insn1@(Mov dest@(X64Op_I (IROp_R _)) src@(X64Op_I (IROp_R _)) _):
   insn2@(Mov dest2@(X64Op_I (IROp_R _)) src2@(X64Op_I (IROp_R _)) ty2):rest) ->
     if dest == src2
       then do
         let newInsn = Mov dest2 src ty2
         runPeephole (insn1:newInsn:rest)
       else do
         rest' <- runPeephole (insn2:rest)
         return (insn1:rest')

  -- mov r0, r0 => kill
  (insn@(Mov dest@(X64Op_I (IROp_R _)) src@(X64Op_I (IROp_R _)) _):rest) ->
     if dest == src
       then runPeephole rest
       else do
         rest' <- runPeephole rest
         return (insn:rest')
         
  (x:xs) -> do
    liftM (x:) (runPeephole xs)

  [] -> return []

