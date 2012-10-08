module Backend.IR.Operand where

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

getImm :: Operand -> Int
getImm (ImmOperand i) = i

