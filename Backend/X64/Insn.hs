module Backend.X64.Insn (
  Scale(..),
  Address(..),
  X64Op(..),
  Cond(..),
  Label(..),
  Insn(..),
  allRegs,
  rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
  r8, r9, r10, r11, r12, r13, r14, r15,
  replaceVReg,
  isBranchInsn
) where

import Data.List (intercalate)
import Backend.IR.IROp

-- X64 reg spec
allRegs = map MReg ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi",
    "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

[rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]
  = allRegs

data Scale = Scale1 | Scale2 | Scale4 | Scale8
  deriving (Eq, Ord)

scaleToInt :: Scale -> Int
scaleToInt Scale1 = 1
scaleToInt Scale2 = 2
scaleToInt Scale4 = 4
scaleToInt Scale8 = 8

instance Show Scale where
  show s = show (scaleToInt s)

data Address = Address Reg (Maybe Reg) Scale Int
  deriving (Eq, Ord)

instance Show Address where
  show (Address base index scale disp) =
    showDisp disp ++ "(" ++ show base ++ "," ++ showIndex index ++ "," ++
    showScale scale ++ ")"
    where
      showDisp :: Int -> String
      showDisp 0 = ""
      showDisp disp = show disp

      showIndex :: Maybe Reg -> String
      showIndex (Just r) = show r
      showIndex Nothing = ""

      showScale :: Scale -> String
      showScale Scale1 = ""
      showScale s = show s

-- Op_I -> IR operand | Op_M -> machine-dependent operand
data X64Op = X64Op_I IROp
           | X64Op_M Address
  deriving (Eq, Ord)

instance Show X64Op where
  show (X64Op_I op) = show op
  show (X64Op_M op) = show op

data Cond = Ge | Gt | Le | Lt | Eq | Ne
  deriving (Show, Eq)

showCond :: Cond -> String
showCond Ge = "ge"
showCond Gt = "g"
showCond Le = "le"
showCond Lt = "l"
showCond Eq = "e"
showCond Ne = "ne"

data Label = Label String
  deriving (Show, Eq)

unLabel :: Label -> String
unLabel (Label label) = label

showLabel :: Label -> String
showLabel = (++":") . unLabel

data Insn = Add X64Op X64Op
          | Sub X64Op X64Op
          | Call Label
          | Cmp X64Op X64Op
          | J Cond Label
          | Jmp Label
          | Lea X64Op X64Op
          | Mov X64Op X64Op
          | Push X64Op
          | Pop X64Op
          | Ret
          | BindLabel Label
  deriving (Eq)

formatInsn :: String -> [String] -> String
formatInsn insn ops = insn ++ " " ++ intercalate ", " ops

instance Show Insn where
  show insn = case insn of
    (Add dest src) -> formatInsn "add" [show dest, show src]
    (Sub dest src) -> formatInsn "sub" [show dest, show src]
    (Call label) -> formatInsn "call" [showLabel label]
    (Cmp lhs rhs) -> formatInsn "cmp" [show lhs, show rhs]
    (J cond label) -> formatInsn ("j" ++ showCond cond) [unLabel label]
    (Jmp label) -> formatInsn "jmp" [unLabel label]
    (Lea dest src) -> formatInsn "lea" [show dest, show src]
    (Mov dest src) -> formatInsn "mov" [show dest, show src]
    (Push src) -> formatInsn "push" [show src]
    (Pop dest) -> formatInsn "pop" [show dest]
    Ret -> "ret"
    (BindLabel label) -> showLabel label

replaceVReg :: (Reg -> Reg) -> Insn -> Insn
replaceVReg f insn = setOpsOfInsn (map (replaceOp f) (opsOfInsn insn)) insn

opsOfInsn :: Insn -> [X64Op]
opsOfInsn insn = case insn of
  (Add op0 op1) -> [op0, op1]
  (Sub op0 op1) -> [op0, op1]
  (Cmp op0 op1) -> [op0, op1]
  (Lea op0 op1) -> [op0, op1]
  (Mov op0 op1) -> [op0, op1]
  (Push op0)    -> [op0]
  (Pop op0)     -> [op0]
  _             -> []

setOpsOfInsn :: [X64Op] -> Insn -> Insn
setOpsOfInsn ops insn = case (ops, insn) of
  ([op0, op1], (Add _ _)) -> Add op0 op1
  ([op0, op1], (Sub _ _)) -> Sub op0 op1
  ([op0, op1], (Cmp _ _)) -> Cmp op0 op1
  ([op0, op1], (Lea _ _)) -> Lea op0 op1
  ([op0, op1], (Mov _ _)) -> Mov op0 op1
  ([op0]     , (Push _) ) -> Push op0
  ([op0]     , (Pop _)  ) -> Pop op0
  ([]        , insn@_   ) -> insn

replaceOp :: (Reg -> Reg) -> X64Op -> X64Op
replaceOp f op = case op of
  (X64Op_I (IROp_R vReg@(VReg _))) -> X64Op_I (IROp_R $ f vReg)
  (X64Op_M (Address r0 maybeR1 scale imm))
    -> X64Op_M (Address (f r0) (fmap f maybeR1) scale imm)
  op@_ -> op

isBranchInsn :: Insn -> Bool
isBranchInsn insn = case insn of
  (Call _) -> True
  (Cmp _ _) -> True
  (J _ _) -> True
  (Jmp _) -> True
  Ret -> True
  _ -> False

