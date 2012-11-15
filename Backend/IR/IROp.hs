module Backend.IR.IROp (
  Reg(..),
  Imm(..),
  IROp(..),
  isMReg,
  isVReg,
  getReg,
  getImm
) where

data Reg = VReg Int
         | MReg String
  deriving (Eq, Ord)

instance Show Reg where
  show (VReg i) = "%v_" ++ show i
  show (MReg s) = "%" ++ s

data Imm = IVal Int
         | LAddr String  -- address of $label
         | LVal String   -- value of label
         | LTmp Int      -- generated label (used by ifs)
  deriving (Eq, Ord)

instance Show Imm where
  show (IVal i) = "$" ++ show i
  show (LAddr s) = "$" ++ s
  show (LVal s) = s
  show (LTmp i) = ".L" ++ show i

data IROp = IROp_R Reg
          | IROp_I Imm
  deriving (Eq, Ord)

instance Show IROp where
  show (IROp_R r) = show r
  show (IROp_I i) = show i

isMReg :: Reg -> Bool
isMReg (MReg _) = True
isMReg _ = False

isVReg :: Reg -> Bool
isVReg (VReg _) = True
isVReg _ = False

getReg :: IROp -> Reg
getReg (IROp_R r) = r
getReg i = error ("Not a register operand: " ++ show i)

getImm :: IROp -> Imm
getImm (IROp_I i) = i
getImm r = error ("Not a immediate operand: " ++ show r)

