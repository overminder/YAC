module Frontend.Rewrite (
  toNormalForm
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.ObjModel
import Backend.IR.Temp

data RewriteState = RewriteState {
  rsVars :: [(String, Int)],
  rsInits :: [(String, Cell)], -- name, form
  rsFuncs :: [(String, Cell, Cell)] -- name, formals, body
}

empty = RewriteState {
  rsVars = [],
  rsInits = [],
  rsFuncs = []
}

type RewriteGen = StateT RewriteState TempGen

toNormalForm :: Cell -> RewriteGen ()
toNormalForm (List cs) = mapM_ mkToplevel cs

mkToplevel :: Cell -> RewriteGen ()
mkToplevel (List cs) = do
  case cs of
    [Symbol "define", Symbol name] -> addVarDef name 0
    [Symbol "define", Symbol name, List (Symbol "lambda":formals:body)] ->
      addFuncDef name formals body
    [Symbol "define", Symbol name, expr] -> do
      addVarDef name 0
      addVarInit name expr
    e@_ -> error $ "Toplevel only allows definition: " ++ show e

addVarDef :: String -> Int -> RewriteGen ()
addVarDef s i = undefined

addFuncDef :: String -> Cell -> [Cell] -> RewriteGen ()
addFuncDef s f bs = undefined

addVarInit :: String -> Cell -> RewriteGen ()
addVarInit s e = undefined
