module Frontend.Scheme.GenC (
  GenState(..),
  runGenC,
  genToplevel,
  CFunc(..),
  findLocals'
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)

import Frontend.Scheme.AST
import Util.Temp

data GenState
  = GenState {
    allFuncs :: [CFunc],
    lamToGen :: [(String, Expr)]
  }

empty = GenState [] []

type GenC = StateT GenState TempGen

getOneLam :: GenC (Maybe (String, Expr))
getOneLam = do
  st <- get
  case lamToGen st of
    x:xs -> do
      put st{lamToGen=xs}
      return $ Just x
    [] ->
      return Nothing

runGenC :: GenState -> GenC a -> TempGen [CFunc]
runGenC st m = liftM allFuncs (execStateT m st)

genToplevel :: GenC ()
genToplevel = do
  maybeLam <- getOneLam
  case maybeLam of
    Just (name, ELambda upv args body) -> do
      cFunc <- execStateT (genLam body) (CFunc name (mkLoc args upv) [])
      modify $ \st -> st {
        allFuncs = cFunc:allFuncs st
      }
      genToplevel
    Nothing -> return ()

addNewLam :: Expr -> GenC String
addNewLam lam = do
  i <- lift nextTemp
  let name = "Lambda_" ++ show i
  modify $ \st -> st {
    lamToGen = (name, lam):lamToGen st
  }
  return name

data CFunc = CFunc {
  cfName :: String,
  cfVars :: Map String Loc,
  cfCode :: [String]
}
  deriving (Show, Eq)

data Loc = Local String
         | Upval Int
  deriving (Show, Eq)

mkLoc :: [String] -> [String] -> Map String Loc
mkLoc locals upvals = Map.fromList (upvalItems ++ localItems)
  where
    upvalItems = zip upvals (map Upval [0..])
    localItems = zip locals (map Local locals)

type GenCFunc = StateT CFunc GenC

emit :: String -> GenCFunc ()
emit line = modify $ \st -> st {
  cfCode = cfCode st ++ [line]
}

emitNewVar :: String -> GenCFunc ()
emitNewVar name = do
  emit $ "ScmPtr " ++ name ++ ";"

emitTailCall :: String -> [String] -> GenCFunc ()
emitTailCall func args = if length args > 5
  then error $ "emitTailCall: too many args: " ++ show args
  else emit $ "Scm_TailCall" ++ show (length args) ++ "(" ++
              intercalate ", " ([func] ++ args) ++ ");"

gensym :: String -> GenCFunc String
gensym s = do
  i <- lift $ lift nextTemp
  return $ s ++ show i

genLam :: Expr -> GenCFunc ()
genLam expr = case expr of
  EAp func args -> do
    funcVal <- genExpr func
    argVals <- mapM genExpr args
    emitTailCall funcVal argVals
  EIf cond ifTrue ifFalse -> do
    condVal <- genExpr cond
    emit $ "if (" ++ condVal ++ " == Scm_True) {"
    genLam ifTrue
    emit "}"
    emit "else {"
    genLam ifFalse
    emit "}"
  ESeq xs -> case xs of
    x:ys@(_:_) -> do
      genStmt x
      genLam $ ESeq ys
    [x] -> do
      genLam x
    [] -> error $ "genLam: empty ESeq"
  _ -> error $ "genLam: illegal expr: " ++ show expr

genExpr :: Expr -> GenCFunc String
genExpr expr = case expr of
  EVar s -> formatVar s
  EInt i -> return $ "Scm_MkInt(" ++ show i ++ ")"
  EBool b -> return $ if b then "Scm_True" else "Scm_False"
  EUnbound -> return $ "Scm_Unbound"
  EUnspecified -> return $ "Scm_Unspecified"
  thisLam@(ELambda upvals args body) -> do
    lamName <- lift $ addNewLam thisLam
    cloName <- gensym "newClosure_"
    emitNewVar cloName
    allocClosure cloName lamName upvals
    return cloName
  _ -> error $ "genExpr: illegal expr: " ++ show expr

genStmt :: Expr -> GenCFunc ()
genStmt expr = case expr of
  EDefine name form -> do
    emitNewVar name -- XXX
    rhs <- genExpr form
    emit $ name ++ " = " ++ rhs ++ ";"
  ESete name form -> do
    rhs <- genExpr form
    emit $ name ++ " = " ++ rhs ++ ";"
  _ -> error $ "genStmt: illegal stmt: " ++ show expr

allocClosure :: String -> String -> [String] -> GenCFunc ()
allocClosure cloName lamName upvals = do
  let extraSize = 8 * length upvals
  -- inlined alloc
  emit $ cloName ++ " = " ++ formatAlloc ("sizeof(ScmClosure) + " ++
         show extraSize) ++ ";"
  emit $ "if (!" ++ cloName ++ ") {"
  -- if alloc failed
  funcLocals <- findLocals
  emit "Scm_PushGcRoot(thisClosure);"
  forM funcLocals $ \name -> do
    emit $ "Scm_PushGcRoot(" ++ name ++ ");"
  emit "Scm_GcCollect();"
  -- restore
  forM (reverse funcLocals) $ \name -> do
    emit $ name ++ " = Scm_PopGcRoot();"
  emit "thisClosure = Scm_PopGcRoot();"
  emit $ cloName ++ " = " ++ formatAlloc ("sizeof(ScmClosure) + " ++
         show extraSize) ++ ";"
  -- end if
  emit "}"
  -- set type, put funcptr and upvals
  emit $ formatClosureCode cloName ++ " = " ++ lamName ++ ";"
  emit $ formatObjType cloName ++ " = Scm_ClosureType;"
  forM_ (zip [0..] upvals) $ \(i, name) -> do
    fromVar <- formatVar name
    emit $ formatUpval cloName i ++ " = " ++ fromVar ++ ";"

formatAlloc :: String -> String
formatAlloc size = "Scm_GcAlloc(" ++ size ++ ")"

formatUpval :: String -> Int -> String
formatUpval cloName i = "ScmClosure_UpvalAt(" ++ cloName ++ ", " ++
                        show i ++ ")"

formatClosureCode :: String -> String
formatClosureCode cloName = "ScmClosure_Code(" ++ cloName ++ ")"

formatObjType :: String -> String
formatObjType obName = "Scm_HeapObjType(" ++ obName ++ ")"

formatVar :: String -> GenCFunc String
formatVar name = do
  varMap <- liftM cfVars get
  return $ case Map.lookup name varMap of
    Just (Local s) -> s
    Just (Upval i) -> formatUpval "thisClosure" i
    Nothing -> name

findLocals' :: CFunc -> [String]
findLocals' = map unLocal . filter isLocal . Map.elems . cfVars
  where
    isLocal (Local _) = True
    isLocal _ = False
    unLocal (Local s) = s

findLocals :: GenCFunc [String]
findLocals = liftM findLocals' get

