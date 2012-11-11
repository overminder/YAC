module Frontend.IRGen (
  IRGen(..),
  gen
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.ObjModel
import Backend.IR.Oprnd
import Backend.IR.Temp
import Backend.IR.Tree

type SymTab = Map String Reg
type IRGen = StateT SymTab TempGen

getNextId :: IRGen Int
getNextId = lift nextTemp

putSymTab :: SymTab -> IRGen ()
putSymTab = put

getSymTab :: IRGen SymTab
getSymTab = get

lookupSymbol :: String -> IRGen (Maybe Reg)
lookupSymbol name = liftM (Map.lookup name) getSymTab

memorizeSymbol :: String -> IRGen Reg
memorizeSymbol name = do
  maybeReg <- lookupSymbol name
  case maybeReg of
    Just reg -> return reg
    Nothing -> do
      tab <- getSymTab
      newReg <- liftM VReg getNextId
      putSymTab $ Map.insert name newReg tab
      return newReg

gen :: Cell -> TempGen Tree
gen c = evalStateT (genWith (Pair (Symbol "begin") c)) Map.empty

genWith :: Cell -> IRGen Tree
genWith (Fixnum i) = return $ Leaf $ ImmOp i
genWith (Symbol name) = do
  maybeReg <- lookupSymbol name
  case maybeReg of
    Just reg -> return $ Leaf $ RegOp reg
    Nothing -> error ("Refering to an unbound variable: " ++ name)
genWith p@(Pair _ _) = case pairToList p of
  (lst, Nil) -> genWithList lst
  otherwise -> error ("Not a proper list to evaluate: " ++ show p)

genWithList :: [Cell] -> IRGen Tree
genWithList [Symbol "+", lhs, rhs] = do
  lTree <- genWith lhs
  rTree <- genWith rhs
  return $ Add lTree rTree
genWithList lst@[Symbol "define", Symbol name, expr] = do
  maybeReg <- lookupSymbol name
  case maybeReg of
    Just reg -> error ("Redefining variable at #define: "
                       ++ (show $ listToCell lst))
    Nothing -> do
      exprTree <- genWith expr
      reg <- memorizeSymbol name
      return $ Move (Leaf $ RegOp reg) exprTree
genWithList lst@[Symbol "set!", Symbol name, expr] = do
  maybeReg <- lookupSymbol name
  case maybeReg of
    Just reg -> do
      exprTree <- genWith expr
      return $ Move (Leaf $ RegOp reg) exprTree
    Nothing -> error ("Unbound variable at #set!: " ++ (show $ listToCell lst))
genWithList ((Symbol "begin"):xs) = do
  trees <- mapM genWith xs
  return $ foldr Seq Nop trees

