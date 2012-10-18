module Frontend.IRGen (
  GenState(..),
  gen
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.ObjModel
import Backend.IR.Operand
import Backend.IR.Tree

type SymTab = Map String Reg

data GenState = GenState {
  nextId :: Int,
  symTab :: SymTab
}

type IRGen = State GenState

emptyGenState = GenState 1 Map.empty

getNextId :: IRGen Int
getNextId = do
  s <- get
  let i = nextId s
  put s{nextId=i+1}
  return i

putSymTab :: SymTab -> IRGen ()
putSymTab tab = do
  s <- get
  put s{symTab=tab}

lookupSymbol :: String -> IRGen (Maybe Reg)
lookupSymbol name = liftM (Map.lookup name . symTab) get

memorizeSymbol :: String -> IRGen Reg
memorizeSymbol name = do
  maybeReg <- lookupSymbol name
  case maybeReg of
    Just reg -> return reg
    Nothing -> do
      tab <- liftM symTab get
      newReg <- liftM PseudoReg getNextId
      putSymTab $ Map.insert name newReg tab
      return newReg

gen :: Cell -> (Tree, GenState)
gen c = runState (genWith (Pair (Symbol "begin") c)) emptyGenState

genWith :: Cell -> IRGen Tree
genWith (Fixnum i) = return $ Leaf $ ImmOperand i
genWith (Symbol name) = do
  maybeReg <- lookupSymbol name
  case maybeReg of
    Just reg -> return $ Leaf $ RegOperand reg
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
      return $ Move (Leaf $ RegOperand reg) exprTree
genWithList lst@[Symbol "set!", Symbol name, expr] = do
  maybeReg <- lookupSymbol name
  case maybeReg of
    Just reg -> do
      exprTree <- genWith expr
      return $ Move (Leaf $ RegOperand reg) exprTree
    Nothing -> error ("Unbound variable at #set!: " ++ (show $ listToCell lst))
genWithList ((Symbol "begin"):xs) = do
  trees <- mapM genWith xs
  return $ foldr Seq Nop trees

