module Frontend.IRGen (
  IRGen,
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
                 | QuadDef String (Maybe Int) -- name initialValue
                 | StringDef String String -- name value
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

newVReg :: IRGen Reg
newVReg = lift $ liftM VReg nextTemp

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
      newReg <- newVReg
      putSymTab $ Map.insert name newReg tab
      return newReg

gen :: Cell -> TempGen [ToplevelDef]
gen c = liftM defs $ execStateT (gen' c) empty

gen' :: Cell -> IRGen ()
gen' c = case c of
  (List xs) -> forM_ xs $ \x -> do
    def <- genToplevel x
    putDef def
  _ -> error $ "IRGen.gen': " ++ show c

genToplevel :: Cell -> IRGen ToplevelDef
genToplevel c = case c of
  (List [Symbol "define", Symbol name]) -> return $ QuadDef name Nothing
  (List [Symbol "define", Symbol name,
         List (Symbol "lambda":formals:body)]) -> do
    formalRegs <- defineFormalArgs formals
    bodyTree <- liftM (T.Return . T.fromList) $ mapM genWith body
    clearSymTab
    return $ FuncDef name formalRegs bodyTree
  (List [Symbol "define", Symbol name, form]) -> case form of
    (Fixnum i) -> return $ QuadDef name (Just i)
    (MString s) -> return $ StringDef name s
  _ -> error $ "IRGen.genToplevel: illegal form: " ++ show c

defineFormalArgs :: Cell -> IRGen [Reg]
defineFormalArgs c = case c of
  (List xs) -> forM xs $ \x -> case x of
      (Symbol s) -> do
        shouldBeNothing <- lookupSymbol s
        case shouldBeNothing of
          Nothing -> memorizeSymbol s
          (Just _) -> error $ "IRGen.defineFormalArgs: redefination of " ++ s
      _ -> error $ "IRGen.defineFormalArgs: not a symbol: " ++ show xs
  _ -> error $ "IRGen.getFormalRegs: illegal args: " ++ show c

genWith :: Cell -> IRGen Tree
genWith c = case c of
  (Fixnum i) -> return $ T.Leaf $ IROp_I $ IVal i
  (Symbol name) -> do
    maybeReg <- lookupSymbol name
    case maybeReg of
      Just reg -> return $ T.Leaf $ IROp_R reg
      Nothing -> error ("Refering to an unbound variable: " ++ name)
  (List xs) -> case xs of
    [] -> error "Unexpected nil in source code: ()"
    _ -> genWithList xs
  _ -> error $ "IRGen.genWith: " ++ show c

genWithList :: [Cell] -> IRGen Tree
genWithList c = case c of
  -- (+ ...)
  ((Symbol "+"):xs) -> case xs of
    [] -> return $ T.Leaf (IROp_I (IVal 0))
    [x] -> genWith x
    _ -> do
      (t0:t1:ts) <- mapM genWith xs
      return $ foldl T.Add (T.Add t0 t1) ts

  ((Symbol "-"):xs) -> case xs of
    [] -> error $ "empty " ++ show c
    [x] -> do
      t <- genWith x
      return $ T.Sub (T.Leaf $ IROp_I $ IVal 0) t
    (y:ys) -> do
      t <- genWith y
      ts <- mapM genWith ys
      return $ foldl T.Sub t ts

  [Symbol "<", lhs, rhs] -> do
    lhsTree <- genWith lhs
    rhsTree <- genWith rhs
    return $ T.Compare lhsTree rhsTree T.Lt

  [Symbol "%symbol-addr", Symbol name] -> do
    return $ T.Leaf $ IROp_I $ LAddr name

  [Symbol "%symbol-val", Symbol name] -> do
    return $ T.Leaf $ IROp_I $ LVal name

  -- (define name expr)
  lst@[Symbol "define", Symbol name, expr] -> do
    maybeReg <- lookupSymbol name
    case maybeReg of
      Just _ -> error ("Redefining variable at #define: "
                       ++ (show $ List lst))
      Nothing -> do
        exprTree <- genWith expr
        reg <- memorizeSymbol name
        return $ T.Move (T.Leaf $ IROp_R reg) exprTree

  [Symbol "if", predExpr, thenExpr, elseExpr] -> do
    [predTree, thenTree, elseTree] <- mapM genWith
                                           [predExpr, thenExpr, elseExpr]
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

  -- (call label args)
  (Symbol "%funcall":Symbol name:args) -> do
    argTrees <- mapM genWith args
    return $ T.Call (T.Leaf $ IROp_I $ LAddr name) argTrees T.NormalCall

  -- (tailcall label args)
  (Symbol "%funcall/t":Symbol name:args) -> do
    argTrees <- mapM genWith args
    return $ T.Call (T.Leaf $ IROp_I $ LAddr name) argTrees T.TailCall

  -- (call expr args)
  (Symbol "%funcall/r":expr:args) -> do
    argTrees <- mapM genWith args
    funcTree <- genWith expr
    return $ T.Call funcTree argTrees T.NormalCall

  -- (tailcall expr args)
  (Symbol "%funcall/rt":expr:args) -> do
    argTrees <- mapM genWith args
    funcTree <- genWith expr
    return $ T.Call funcTree argTrees T.TailCall

  [Symbol "%set-quad!", ptr, offset, val] -> do
    ptrTree <- genWith ptr
    offTree <- genWith offset
    valTree <- genWith val
    return $ T.Move (T.Deref (T.Add ptrTree offTree)) valTree

  [Symbol "%get-quad", ptr, offset] -> do
    ptrTree <- genWith ptr
    offTree <- genWith offset
    return $ T.Deref (T.Add ptrTree offTree)

  [Symbol "%sal", lhs, rhs] -> do
    lTree <- genWith lhs
    rTree <- genWith rhs
    return $ T.Shift lTree rTree T.ToLeft

  [Symbol "%sar", lhs, rhs] -> do
    lTree <- genWith lhs
    rTree <- genWith rhs
    return $ T.Shift lTree rTree T.ToRight

  [Symbol "%bitand", lhs, rhs] -> do
    lTree <- genWith lhs
    rTree <- genWith rhs
    return $ T.BitAnd lTree rTree

  [Symbol "%bitor", lhs, rhs] -> do
    lTree <- genWith lhs
    rTree <- genWith rhs
    return $ T.BitOr lTree rTree

  -- otherwise
  xs@_ -> error $ "Unexpected form: " ++ show (List xs)

