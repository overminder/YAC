module Frontend.Scheme.Lowlevel (
  LLType(..),
  AccessPath(..),
  Access(..),
  mkLLOp,
) where

import qualified Backend.IR.Tree as T
import Backend.IR.IROp

data LLType
  = LLStruct {
    tyName :: String,
    tyFields :: [LLType]
  }
  | LLArray {
    itemType :: LLType
  }
  | LLPtr {
    pointsTo :: LLType
  }
  | LLInt {
    intWidth :: Int
  }
  deriving (Show, Eq)

data AccessPath = AccessPath LLType [Access]
  deriving (Show, Eq)

data Access
  = GetField Int
  | Deref
  deriving (Show, Eq)

-- sizeOf in bytes
sizeOf :: LLType -> Int
sizeOf ty = case ty of
  LLStruct _ fields = sum (map sizeOf fields)
  LLArray itemTy = 0 -- sizeOf itemTy
  LLPtr _ = sizeOf int64
  LLInt w = w

offsetOf :: [LLType] -> Int -> Int
offsetOf tys i = sum (map sizeof take (i - 1) tys)

mkLLOp' :: T.Tree -> LLType -> Access -> (T.Tree, LLType)
mkLLOp' t (LLStruct name fields) (GetField i) =
  (T.Add t (T.Leaf $ IROp_I $ IVal offset), fields !! i)
  where
    offset = offsetOf fields i

mkLLOp' t (LLPtr ty) Deref = (T.Deref t, ty)

mkLLOp :: T.Tree -> AccessPath -> T.Tree
mkLLOp t (AccessPath ty chain) = foldl combine (t, ty) chain
  where
    combine (currTree, currTy) access = mkLLOp' currTree currTy access

int8  = LLInt 1
int16 = LLInt 2
int32 = LLInt 4
int64 = LLInt 8

heapObj = LLStruct "HeapObjectHeader" [int64]

boxedInt = LLStruct {
  tyName = "Int",
  tyFields = [heapObj, int64]
}

intGetter = AccessPath boxedInt 1

intPrinter :: T.Tree -> T.Tree
intPrinter t = T.Call (T.Leaf $ IROp_I $ LAddr "printf")
                      [(mkLLOp t intGetter)]

pair = LLstruct {
  tyName = "Pair",
  tyFields = [heapObj, LLPtr heapObj , LLPtr heapObj]
}

carGetter = AccessPath pair 1
cdrGetter = AccessPath pair 2

charArray = LLArray int8

symbol = LLStruct {
  tyName = "Symbol",
  tyFields = [heapObj, charArray]
}


