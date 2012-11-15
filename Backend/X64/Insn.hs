module Backend.X64.Insn (
  Scale(..),
  Address(..),
  X64Op(..),
  Insn(..),
  MovType(..),
  PseudoInsn(..),
  GasSyntax(..),
  allRegs,
  rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
  r8, r9, r10, r11, r12, r13, r14, r15, rip,
  replaceVReg,
  isBranchInsn,
  isBranchTarget,
  getBranchTarget,
  mightFallThrough,
) where

import Data.List (intercalate)
import Backend.IR.IROp
import Backend.IR.Tree (Cond(..))

-- X64 reg spec
allRegs :: [Reg]
allRegs = map MReg ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi",
    "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "rip"]

rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi :: Reg
r8, r9, r10, r11, r12, r13, r14, r15, rip :: Reg
[rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
 r8, r9, r10, r11, r12, r13, r14, r15, rip] = allRegs

data Scale = Scale1 | Scale2 | Scale4 | Scale8
  deriving (Eq, Ord)

scaleToInt :: Scale -> Int
scaleToInt Scale1 = 1
scaleToInt Scale2 = 2
scaleToInt Scale4 = 4
scaleToInt Scale8 = 8

instance Show Scale where
  show s = show (scaleToInt s)

data Address = Address Reg (Maybe Reg) Scale Imm
  deriving (Eq, Ord)

instance Show Address where
  show (Address base index scale disp) =
    showDisp disp ++ "(" ++ show base ++ showIndex index ++ 
    showScale scale ++ ")"
    where
      showDisp :: Imm -> String
      showDisp imm = case imm of
        (IVal i) -> case i of
          0 -> ""
          _ -> show i
        (LVal s) -> s
        _ -> error $ "X64.Insn.showDisp: wrong disp: " ++ show imm

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

showCond :: Cond -> String
showCond Ge = "ge"
showCond Gt = "g"
showCond Le = "le"
showCond Lt = "l"
showCond Eq = "e"
showCond Ne = "ne"

class GasSyntax a where
  gasShow :: a -> String

data Insn = Add X64Op X64Op
          | Sub X64Op X64Op
          | Call X64Op
          | Cmp X64Op X64Op
          | J Cond X64Op
          | Jmp X64Op
          | Lea X64Op X64Op
          | Mov X64Op X64Op MovType
          | Push X64Op
          | Pop X64Op
          | Ret
          | PInsn PseudoInsn
          | BindLabel X64Op
  deriving (Eq)

data MovType = NormalMov
             | CalleeMovArg
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
    (Call dest) -> formatInsn "call" [showJumpTarget dest]
    (Cmp lhs rhs) -> formatInsn "cmp" [show lhs, show rhs]
    (J cond label) -> formatInsn ("j" ++ showCond cond) [showJumpTarget label]
    (Jmp dest) -> formatInsn "jmp" [showJumpTarget dest]
    (Lea dest src) -> formatInsn "lea" [show dest, show src]
    (Mov dest src _) -> formatInsn "mov" [show dest, show src]
    (Push src) -> formatInsn "push" [show src]
    (Pop dest) -> formatInsn "pop" [show dest]
    (PInsn p) -> formatInsn (show p) []
    Ret -> "ret"
    (BindLabel label) -> showJumpTarget label ++ ":"

instance GasSyntax Insn where
  gasShow insn = case insn of
    (Add dest src) -> formatInsn "add" [show src, show dest]
    (Sub dest src) -> formatInsn "sub" [show src, show dest]
    (Call dest) -> formatInsn "call" [showJumpTarget dest]
    (Cmp lhs rhs) -> formatInsn "cmp" [show rhs, show lhs]
    (J cond label) -> formatInsn ("j" ++ showCond cond) [showJumpTarget label]
    (Jmp dest) -> formatInsn "jmp" [showJumpTarget dest]
    (Lea dest src) -> formatInsn "lea" [show src, show dest]
    (Mov dest src _) -> formatInsn "mov" [show src, show dest]
    (Push src) -> formatInsn "push" [show src]
    (Pop dest) -> formatInsn "pop" [show dest]
    Ret -> "ret"
    (BindLabel label) -> showJumpTarget label ++ ":"
    _ -> show insn

showJumpTarget :: X64Op -> String
showJumpTarget op = case op of
  (X64Op_I (IROp_I (LAddr name))) -> name
  (X64Op_I (IROp_I (LTmp i))) -> ".L" ++ show i
  (X64Op_I (IROp_R r)) -> "*" ++ show r
  (X64Op_M addr@(Address _ _ _ _)) -> "*" ++ show addr
  _ -> error $ "unknown jump target: " ++ show op

replaceVReg :: (Reg -> Reg) -> Insn -> Insn
replaceVReg f insn = setOpsOfInsn (map (replaceOp f) (opsOfInsn insn)) insn

opsOfInsn :: Insn -> [X64Op]
opsOfInsn insn = case insn of
  (Add op0 op1) -> [op0, op1]
  (Sub op0 op1) -> [op0, op1]
  (Call op0)    -> [op0]
  (Cmp op0 op1) -> [op0, op1]
  (Jmp op0)     -> [op0]
  (Lea op0 op1) -> [op0, op1]
  (Mov op0 op1 _) -> [op0, op1]
  (Push op0)    -> [op0]
  (Pop op0)     -> [op0]
  (PInsn InsertPrologue) -> []
  (PInsn InsertEpilogue) -> []
  Ret           -> []
  (J _ _)       -> []
  (BindLabel _) -> []
  --_             -> error $ "Insn.opsOfInsn: " ++ show insn

setOpsOfInsn :: [X64Op] -> Insn -> Insn
setOpsOfInsn ops insn = case (ops, insn) of
  ([op0, op1], (Add _ _)) -> Add op0 op1
  ([op0, op1], (Sub _ _)) -> Sub op0 op1
  ([op0]     , (Call _) ) -> Call op0
  ([op0, op1], (Cmp _ _)) -> Cmp op0 op1
  ([op0]     , (Jmp _)  ) -> Jmp op0
  ([op0, op1], (Lea _ _)) -> Lea op0 op1
  ([op0, op1], (Mov _ _ ty)) -> Mov op0 op1 ty
  ([op0]     , (Push _) ) -> Push op0
  ([op0]     , (Pop _)  ) -> Pop op0
  (_         , (PInsn InsertPrologue)) -> insn
  (_         , (PInsn InsertEpilogue)) -> insn
  (_         , Ret      ) -> insn
  (_         , (J _ _))   -> insn
  (_         , (BindLabel _)) -> insn
  _             -> error $ "Insn.setOpsOfInsn: " ++ show insn

replaceOp :: (Reg -> Reg) -> X64Op -> X64Op
replaceOp f op = case op of
  (X64Op_I (IROp_R vReg@(VReg _))) -> X64Op_I (IROp_R $ f vReg)
  (X64Op_M (Address r0 maybeR1 scale imm))
    -> X64Op_M (Address (f r0) (fmap f maybeR1) scale imm)
  _ -> op

isBranchInsn :: Insn -> Bool
isBranchInsn insn = case insn of
  --(Call _) -> True -- ??
  (J _ _) -> True
  (Jmp _) -> True
  Ret -> True
  _ -> False

isBranchTarget :: Insn -> Bool
isBranchTarget insn = case insn of
  (BindLabel _) -> True
  _ -> False

getBranchTarget :: Insn -> Maybe Imm
getBranchTarget insn = case insn of
  (J _ (X64Op_I (IROp_I label))) -> Just label
  (Jmp (X64Op_I (IROp_I label@(LAddr _)))) -> Just label
  _ -> Nothing

mightFallThrough :: Insn -> Bool
mightFallThrough insn = case insn of
  (Jmp _) -> False
  Ret -> False
  _ -> True
  -- For Call, since control will return to the next insn, and
  -- args will be stored before it and restored after it,
  -- we think call acts just like normal insn.


