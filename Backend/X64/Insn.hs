module Backend.X64.Insn where

import Data.List (intercalate)
import qualified Backend.IR.Operand as IROp
import qualified Backend.IR.Tree as IRTree

data Scale = Scale1 | Scale2 | Scale4 | Scale8
  deriving (Eq, Ord)

scaleToInt :: Scale -> Int
scaleToInt Scale1 = 1
scaleToInt Scale2 = 2
scaleToInt Scale4 = 4
scaleToInt Scale8 = 8

instance Show Scale where
  show s = show (scaleToInt s)

data Address = Address IROp.Reg (Maybe IROp.Reg) Scale Int
  deriving (Eq, Ord)

instance Show Address where
  show (Address base index scale disp) =
    showDisp disp ++ "(" ++ show base ++ "," ++ showIndex index ++ "," ++
    showScale scale ++ ")"
    where
      showDisp :: Int -> String
      showDisp 0 = ""
      showDisp disp = show disp

      showIndex :: Maybe IROp.Reg -> String
      showIndex (Just r) = show r
      showIndex Nothing = ""

      showScale :: Scale -> String
      showScale Scale1 = ""
      showScale s = show s

-- Messy
data Operand = IROperand IROp.Operand
             | X64Operand Address
  deriving (Eq, Ord)

instance Show Operand where
  show (IROperand op) = show op
  show (X64Operand op) = show op

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

data Insn = Add Operand Operand
          | Sub Operand Operand
          | Call Label
          | Cmp Operand Operand
          | J Cond Label
          | Jmp Label
          | Lea Operand Operand
          | Mov Operand Operand
          | Push Operand
          | Pop Operand
          | Ret
          | BindLabel Label
  deriving (Eq)

formatInsn :: String -> [String] -> String
formatInsn insn ops = insn ++ " " ++ intercalate ", " ops

instance Show Insn where
  show (Add dest src) = formatInsn "add" [show dest, show src]
  show (Sub dest src) = formatInsn "sub" [show dest, show src]
  show (Call label) = formatInsn "call" [showLabel label]
  show (Cmp lhs rhs) = formatInsn "cmp" [show lhs, show rhs]
  show (J cond label) = formatInsn ("j" ++ showCond cond) [unLabel label]
  show (Jmp label) = formatInsn "jmp" [unLabel label]
  show (Lea dest src) = formatInsn "lea" [show dest, show src]
  show (Mov dest src) = formatInsn "mov" [show dest, show src]
  show (Push src) = formatInsn "push" [show src]
  show (Pop dest) = formatInsn "pop" [show dest]
  show Ret = "ret"
  show (BindLabel label) = showLabel label


