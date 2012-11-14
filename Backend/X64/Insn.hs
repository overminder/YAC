module Backend.X64.Insn (
  Scale(..),
  Address(..),
  X64Op(..),
  Cond(..),
  Label(..),
  Insn(..),
  PseudoInsn(..),
  allRegs,
  rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
  r8, r9, r10, r11, r12, r13, r14, r15,
  replaceVReg,
  isBranchInsn,
  isBranchTarget,
  getBranchTarget,
  mightFallThrough,
  gasShow,
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
    showDisp disp ++ "(" ++ show base ++ showIndex index ++ 
    showScale scale ++ ")"
    where
      showDisp :: Int -> String
      showDisp 0 = ""
      showDisp disp = show disp

      showScale :: Scale -> String
      showScale Scale1 = ""
      showScale s@_ = "," ++ show (scaleToInt s)

      showIndex :: Maybe Reg -> String
      showIndex (Just r) = "," ++ show r
      showIndex Nothing = ""

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

data Label = StringLabel String
           | IntLabel Int
  deriving (Show, Eq, Ord)

unLabel :: Label -> String
unLabel lbl = case lbl of
  (StringLabel s) -> s
  (IntLabel i) -> ".L" ++ show i

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
          | PInsn PseudoInsn
          | BindLabel Label
  deriving (Eq)

data PseudoInsn = InsertPrologue
                | InsertEpilogue
  deriving (Show, Eq)

formatInsn :: String -> [String] -> String
formatInsn insn ops = insn ++ " " ++ intercalate ", " ops

instance Show Insn where
  show insn = case insn of
    (Add dest src) -> formatInsn "add" [show dest, show src]
    (Sub dest src) -> formatInsn "sub" [show dest, show src]
    (Call label) -> formatInsn "call" [unLabel label]
    (Cmp lhs rhs) -> formatInsn "cmp" [show lhs, show rhs]
    (J cond label) -> formatInsn ("j" ++ showCond cond) [unLabel label]
    (Jmp label) -> formatInsn "jmp" [unLabel label]
    (Lea dest src) -> formatInsn "lea" [show dest, show src]
    (Mov dest src) -> formatInsn "mov" [show dest, show src]
    (Push src) -> formatInsn "push" [show src]
    (Pop dest) -> formatInsn "pop" [show dest]
    (PInsn p) -> formatInsn (show p) []
    Ret -> "ret"
    (BindLabel label) -> showLabel label

gasShow :: Insn -> String
gasShow insn = case insn of
  (Add dest src) -> formatInsn "add" [show src, show dest]
  (Sub dest src) -> formatInsn "sub" [show src, show dest]
  (Call label) -> formatInsn "call" [unLabel label]
  (Cmp lhs rhs) -> formatInsn "cmp" [show rhs, show lhs]
  (J cond label) -> formatInsn ("j" ++ showCond cond) [unLabel label]
  (Jmp label) -> formatInsn "jmp" [unLabel label]
  (Lea dest src) -> formatInsn "lea" [show src, show dest]
  (Mov dest src) -> formatInsn "mov" [show src, show dest]
  (Push src) -> formatInsn "push" [show src]
  (Pop dest) -> formatInsn "pop" [show dest]
  Ret -> "ret"
  (BindLabel label) -> showLabel label
  _ -> show insn

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
  (J _ _) -> True
  (Jmp _) -> True
  Ret -> True
  _ -> False

isBranchTarget :: Insn -> Bool
isBranchTarget insn = case insn of
  (BindLabel _) -> True
  _ -> False

getBranchTarget :: Insn -> Maybe Label
getBranchTarget insn = case insn of
  (J _ label) -> Just label
  (Jmp label) -> Just label
  _ -> Nothing

mightFallThrough :: Insn -> Bool
mightFallThrough insn = case insn of
  (Jmp _) -> False
  Ret -> False
  _ -> True
  -- what about Call? Welp, let's ignore it for now.


