module Frontend.IRGen (
  IRGen(..),
  ToplevelDef(..),
  gen
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.ObjModel
import Backend.IR.IROp
import Backend.IR.Temp
import Backend.IR.Tree (Tree)
import qualified Backend.IR.Tree as T

data ToplevelDef = FuncDef String [Reg] Tree -- name args body
                 | VarDef String (Maybe Int) -- name initialValue
  deriving (Show)

data IRGenState = IRGenState {
  symTab :: SymTab,
  defs :: [ToplevelDef]
}

type SymTab = Map String Reg

empty :: IRGenState
empty = IRGenState {
  symTab = Map.empty,
  defs = []
}

type IRGen = StateT IRGenState TempGen

getNextId :: IRGen Int
getNextId = lift nextTemp

putSymTab :: SymTab -> IRGen ()
putSymTab tab = modify $ \st -> st {
  symTab = tab
}

getSymTab :: IRGen SymTab
getSymTab = liftM symTab get

clearSymTab :: IRGen ()
clearSymTab = putSymTab Map.empty

putDef :: ToplevelDef -> IRGen ()
putDef d = modify $ \st -> st {
  defs = d:defs st
}

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

gen :: Cell -> TempGen [ToplevelDef]
gen c = liftM defs $ execStateT (gen' c) empty

gen' :: Cell -> IRGen ()
gen' (List cs) = do
  forM_ cs $ \c -> do
    def <- genToplevel c
    putDef def

genToplevel :: Cell -> IRGen ToplevelDef
genToplevel c = case c of
  (List [Symbol "define", Symbol name]) -> return $ VarDef name Nothing
  (List [Symbol "define", Symbol name,
         List (Symbol "lambda":formals:body)]) -> do
    formalRegs <- defineFormalArgs formals
    bodyTree <- liftM (T.Return . T.fromList) $ mapM genWith body
    clearSymTab
    return $ FuncDef name formalRegs bodyTree
  (List [Symbol "define", Symbol name, Fixnum i]) -> do
    return $ VarDef name (Just i)
  _ -> error $ "IRGen.genToplevel: illegal form: " ++ show c

defineFormalArgs :: Cell -> IRGen [Reg]
defineFormalArgs c = case c of
  (List xs) -> forM xs $ \(Symbol s) -> do
    shouldBeNothing <- lookupSymbol s
    case shouldBeNothing of
      Nothing -> memorizeSymbol s
      (Just _) -> error $ "IRGen.defineFormalArgs: redefination of " ++ s
  _ -> error $ "IRGen.getFormalRegs: illegal args: " ++ show c

genWith :: Cell -> IRGen Tree
genWith c = case c of
  (Fixnum i) -> return $ T.Leaf $ IROp_I i
  (Symbol name) -> do
    maybeReg <- lookupSymbol name
    case maybeReg of
      Just reg -> return $ T.Leaf $ IROp_R reg
      Nothing -> error ("Refering to an unbound variable: " ++ name)
  (List xs) -> case xs of
    [] -> error "Unexpected nil in source code: ()"
    _ -> genWithList xs

genWithList :: [Cell] -> IRGen Tree
genWithList c = case c of
  -- (+ ...)
  ((Symbol "+"):xs) -> case xs of
    [] -> return $ T.Leaf (IROp_I 0)
    [x] -> genWith x
    _ -> do
      (t0:t1:ts) <- mapM genWith xs
      return $ foldl T.Add (T.Add t0 t1) ts

  -- (define name expr)
  lst@[Symbol "define", Symbol name, expr] -> do
    maybeReg <- lookupSymbol name
    case maybeReg of
      Just reg -> error ("Redefining variable at #define: "
                         ++ (show $ List lst))
      Nothing -> do
        exprTree <- genWith expr
        reg <- memorizeSymbol name
        return $ T.Move (T.Leaf $ IROp_R reg) exprTree

  [Symbol "if", pred, thenExpr, elseExpr] -> do
    [predTree, thenTree, elseTree] <- mapM genWith [pred, thenExpr, elseExpr]
    return $ T.If predTree thenTree elseTree

  -- (set! name expr)
  lst@[Symbol "set!", Symbol name, expr] -> do
    maybeReg <- lookupSymbol name
    case maybeReg of
      Just reg -> do
        exprTree <- genWith expr
        return $ T.Move (T.Leaf $ IROp_R reg) exprTree
      Nothing -> error ("Unbound variable at #set!: " ++ (show $ List lst))

  -- (begin ...)
  (Symbol "begin":xs) -> do
    trees <- mapM genWith xs
    return $ T.fromList trees

  -- (funcall name args)
  (Symbol "funcall":Symbol name:args) -> do
    argTrees <- mapM genWith args
    return $ T.Call (T.Leaf $ IROp_L name) argTrees

  -- otherwise
  xs@_ -> error $ "Unexpected form: " ++ show (List xs)

