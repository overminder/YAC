{-# LANGUAGE TypeSynonymInstances #-}

module Frontend.Scheme.AST (
  toAST,
  runASTGen,
  AbstractExpr(..),
  Expr,
  flattenESeq,
  isAtom
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)

import Frontend.ObjModel
import Util.Temp
import qualified Util.Ppr as Ppr

type ToplevelDefn = Map String Expr

data ASTState
  = ASTState {
    defns :: ToplevelDefn,
    mainBody :: [Expr]
  }
  deriving (Show, Eq)

empty = ASTState {
  defns = Map.empty,
  mainBody = []
}

type ASTGen = StateT ASTState TempGen

data AbstractExpr a
  -- Atoms
  = EVar a
  | EInt Int
  | EBool Bool
  | EUnbound
  | EUnspecified
  -- Complex exprs
  | ELambda [a] [a] (AbstractExpr a) -- capture \args body
  | EAp (AbstractExpr a) [AbstractExpr a]
  | EIf (AbstractExpr a) (AbstractExpr a) (AbstractExpr a)
  | ESeq [AbstractExpr a]
  | EDefine a (AbstractExpr a)
  | ESete a (AbstractExpr a)
  deriving (Show, Eq)

instance Functor AbstractExpr where
  fmap f expr = case expr of
    EVar a -> EVar (f a)
    ELambda as1 as2 e -> ELambda (fmap f as1) (fmap f as2) (fmap f e)
    EAp e es -> EAp (fmap f e) (map (fmap f) es)
    EIf e1 e2 e3 -> EIf (fmap f e1) (fmap f e2) (fmap f e3)
    ESeq es -> ESeq (map (fmap f) es)
    EDefine a e -> EDefine (f a) (fmap f e)
    ESete a e -> ESete (f a) (fmap f e)
    EInt i -> EInt i
    EBool b -> EBool b
    EUnbound -> EUnbound
    EUnspecified -> EUnspecified

type Expr = AbstractExpr String

instance Ppr.Ppr Expr where
  render e = case e of
    EVar s -> Ppr.write s
    EInt i -> Ppr.write $ show i
    EBool b -> Ppr.write $ show b
    EUnbound -> Ppr.write "#<Unbound>"
    EUnspecified -> Ppr.write "#<Unspecified>"
    ELambda upvals args body -> do
      if not $ null upvals
        then do
          Ppr.write "{"
          Ppr.write $ intercalate " " upvals
          Ppr.write "}"
        else
          return ()
      if not $ null args
        then do
          Ppr.write "\\"
          Ppr.write $ intercalate " " args
          Ppr.write " "
        else return ()
      Ppr.writeLn "-> {"
      Ppr.indent 2
      Ppr.render body
      Ppr.indent (-2)
      Ppr.writeLn "}"
    EAp f args -> do
      Ppr.write "("
      Ppr.render f
      Ppr.write " "
      forM_ (zip [0..] args) $ \(i, arg) -> do
        if i /= 0
          then Ppr.write " "
          else return ()
        Ppr.render arg
      Ppr.write ")"
    EIf cond ifTrue ifFalse -> do
      Ppr.write "If "
      Ppr.render cond
      Ppr.newLine
      Ppr.indent 2

      Ppr.write "Then {"
      Ppr.render ifTrue
      Ppr.writeLn "}"

      Ppr.write "Else {"
      Ppr.render ifFalse
      Ppr.write "}"

      Ppr.indent (-2)
    ESeq xs -> do
      forM_ xs $ \x -> do
        Ppr.render x
    EDefine name form -> do
      Ppr.write "Define "
      Ppr.write name
      Ppr.write " := "
      Ppr.render form
      Ppr.newLine
    ESete name form -> do
      Ppr.write "Set "
      Ppr.write name
      Ppr.write " <- "
      Ppr.render form
      Ppr.newLine

flattenESeq :: Expr -> [Expr]
flattenESeq e = case e of
  ESeq xs -> concatMap flattenESeq xs
  _ -> [e]

isAtom :: Expr -> Bool
isAtom e = case e of
  EVar _ -> True
  EInt _ -> True
  EBool _ -> True
  EUnbound -> True
  EUnspecified -> True
  _ -> False

runASTGen :: ASTGen a -> TempGen a
runASTGen m = evalStateT m empty

toAST :: [Cell] -> ASTGen ToplevelDefn
toAST cs = do
  mapM_ genToplevelStmt cs
  makeMain
  liftM defns get

type Env = [String] -- what local variable do I have
emptyEnv = []

makeMain :: ASTGen ()
makeMain = do
  mainExprs <- liftM mainBody get
  addDefn "Main:main" (ELambda [] [] (ESeq mainExprs))

addDefn :: String -> Expr -> ASTGen ()
addDefn name expr = modify $ \st -> st {
  defns = Map.insert name expr (defns st)
}

addToMain :: Expr -> ASTGen ()
addToMain expr = modify $ \st -> st {
  mainBody = mainBody st ++ [expr]
}

data DefContext = Toplevel | Local
  deriving (Show, Eq)

genToplevelStmt :: Cell -> ASTGen ()
genToplevelStmt c = case c of
  List [Symbol "define", Symbol name, form] -> do
    addDefn name EUnbound
    addToMain (ESete name (toExpr form))
  List (Symbol "define":List (Symbol name:args):body) -> do
    addDefn name (ELambda [] (map symToStr args)
                          (ESeq (map toExpr body)))
  _ -> addToMain (toExpr c) -- XXX

toExpr :: Cell -> Expr
toExpr c = case c of
  List [Symbol "define", Symbol name, form] ->
    EDefine name (toExpr form)
  List (Symbol "define":List (Symbol name:args):body) ->
    EDefine name (ELambda [] (map symToStr args) (ESeq (map toExpr body)))
  List (Symbol "lambda":List args:body) ->
    (ELambda [] (map symToStr args) (ESeq (map toExpr body)))
  List [Symbol "set!", Symbol name, form] ->
    ESete name (toExpr form)
  List [Symbol "if", cond, ifTrue, ifFalse] ->
    EIf (toExpr cond) (toExpr ifTrue) (toExpr ifFalse)
  List [Symbol "if", cond, ifTrue] ->
    EIf (toExpr cond) (toExpr ifTrue) EUnspecified
  List (func:args) ->
    EAp (toExpr func) (map toExpr args)
  Symbol name -> EVar name
  Fixnum i -> EInt i
  Boolean b -> EBool b
  _ -> error $ "Unknown expr: " ++ show c

symToStr :: Cell -> String
symToStr c = case c of
  Symbol name -> name
  _ -> error $ "symToStr: not a symbol " ++ show c

