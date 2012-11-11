module Backend.IR.Oprnd (
  Reg(..),
  Oprnd(..),
  getReg,
  getImm
) where

data Reg = VReg Int
         | MReg String
  deriving (Eq, Ord)

instance Show Reg where
  show (VReg i) = "%v_" ++ show i
  show (MReg s) = "%" ++ s

data Oprnd = RegOp Reg
           | ImmOp Int
  deriving (Eq, Ord)

instance Show Oprnd where
  show (RegOp r) = show r
  show (ImmOp i) = "$" ++ show i

getReg :: Oprnd -> Reg
getReg (RegOp r) = r
getReg i = error ("Not a register operand: " ++ show i)

getImm :: Oprnd -> Int
getImm (ImmOp i) = i
getImm r = error ("Not a immediate operand: " ++ show r)

