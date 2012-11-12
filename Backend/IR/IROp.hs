module Backend.IR.IROp (
  Reg(..),
  IROp(..),
  getReg,
  getImm
) where

data Reg = VReg Int
         | MReg String
  deriving (Eq, Ord)

instance Show Reg where
  show (VReg i) = "%v_" ++ show i
  show (MReg s) = "%" ++ s

data IROp = IROp_R Reg
          | IROp_I Int
  deriving (Eq, Ord)

instance Show IROp where
  show (IROp_R r) = show r
  show (IROp_I i) = "$" ++ show i

getReg :: IROp -> Reg
getReg (IROp_R r) = r
getReg i = error ("Not a register operand: " ++ show i)

getImm :: IROp -> Int
getImm (IROp_I i) = i
getImm r = error ("Not a immediate operand: " ++ show r)

