module Backend.IR.Operand (
  Reg(..),
  Operand(..),
  getReg,
  getImm
) where

data Reg = PseudoReg Int
         | MachReg String
  deriving (Eq, Ord)

instance Show Reg where
  show (PseudoReg i) = "%pr" ++ show i
  show (MachReg s) = "%" ++ s

data Operand = RegOperand Reg
             | ImmOperand Int
  deriving (Eq, Ord)

instance Show Operand where
  show (RegOperand r) = show r
  show (ImmOperand i) = "$" ++ show i

getReg :: Operand -> Reg
getReg (RegOperand r) = r
getReg i = error ("Not a register operand: " ++ show i)

getImm :: Operand -> Int
getImm (ImmOperand i) = i
getImm r = error ("Not a immediate operand: " ++ show r)

