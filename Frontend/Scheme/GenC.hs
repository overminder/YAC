module Frontend.Scheme.GenC (
  GenState(..),
  runGenC,
  mkGenState,
  genToplevel,
  CFunc(..),
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate, partition)

import Frontend.Scheme.AST
import Frontend.Scheme.Mangler
import Util.Temp

class CSource a where
  toC :: a -> String

data GenState
  = GenState {
    cFuncDefs :: [CFunc],
    cTopDecls :: [String],
    lamToGen :: [(String, Expr)]
  }

instance CSource GenState where
  toC st = unlines $ cTopDecls st ++ map toC (cFuncDefs st)

type GenC = StateT GenState TempGen

comma_join :: [String] -> String
comma_join = intercalate ", "

getOneLam :: GenC (Maybe (String, Expr))
getOneLam = do
  st <- get
  case lamToGen st of
    x:xs -> do
      put st{lamToGen=xs}
      return $ Just x
    [] ->
      return Nothing

runGenC :: GenState -> GenC a -> TempGen String
runGenC initState m = do
  st <- execStateT m initState
  return $ toC st

mkGenState :: [(String, Expr)] -> GenState
mkGenState topDefs = GenState [] (concatMap mkSuperComb topLams ++
                                  map mkVarDef topVars ++
                                  globalGcRootAdder)
                                 (map addLamPrefix topLams)
  where
    topNames = map fst topDefs
    globalGcRootAdder
      = ["static void AddGlobalGcRoots() {"] ++
        ["Scm_AddGlobalGcRoot(&" ++ name ++ ");" | name <- topNames] ++
        ["}"]
    (topLams, topVars) = partition (isLambda . snd) topDefs
    mkVarDef (name, expr) = "ScmPtr " ++ name ++ " = " ++ genAtom expr ++ ";"
    mkSuperComb (name, ELambda _ args _)
      = ["static void Sc_" ++ name ++ "(" ++ formatArgs args ++ ");",
         "static ScmClosure _" ++ name ++
         " = Scm_MkSuperComb(" ++
         comma_join ["Sc_" ++ name, show $ demangle name,
                     show $ length args] ++ ");",
         "ScmPtr " ++ name ++ " = (ScmPtr) &_" ++ name ++ ";"]
    addLamPrefix (name, expr) = ("Sc_" ++ name, expr)
    isLambda (ELambda _ _ _) = True
    isLambda _ = False

curr_closure = "thisClosure"

formatArgs :: [String] -> String
formatArgs args = intercalate ", " $ map ("ScmPtr "++) (curr_closure:args)

genToplevel :: GenC ()
genToplevel = do
  maybeLam <- getOneLam
  case maybeLam of
    Just (name, ELambda upv args body) -> do
      let initCode = if name == "Sc_Mainzkmain"
                       then ["AddGlobalGcRoots();"]
                       else []
      let funcToGen = CFunc {cfName=name, cfArgs=args,
                             cfVars=mkLoc args upv, cfCode=initCode,
                             cfExtraLocals=[]}
      cFunc <- execStateT (genLam body) funcToGen
      modify $ \st -> st {
        cFuncDefs = cFunc:cFuncDefs st
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
  cfArgs :: [String],
  cfVars :: Map String Loc,
  cfCode :: [String],
  cfExtraLocals :: [String]
}
  deriving (Show, Eq)

instance CSource CFunc where
  toC cFunc@(CFunc name args vars code _)
    = unlines (["static void",
                name ++ "(" ++ (formatArgs args)
                ++ ") {"] ++ code ++ ["}"])

data Loc = Local String
         | Upval Int
  deriving (Show, Eq)

addExtraLocal :: String -> GenCFunc ()
addExtraLocal name = modify $ \st -> st {
  cfExtraLocals = name : cfExtraLocals st
}

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

emitStmt :: String -> GenCFunc ()
emitStmt stmt = emit $ stmt ++ ";"

emitNewVar :: String -> GenCFunc ()
emitNewVar name = do
  emit $ "ScmPtr " ++ name ++ ";"

emitTailCall :: String -> [String] -> GenCFunc ()
emitTailCall func args = if length args > 5
  then error $ "emitTailCall: too many args: " ++ show args
  else do
    emitStmt $ "Scm_TailCall" ++ show (length args) ++ "(" ++
               intercalate ", " ([func] ++ args) ++ ")"

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
  EVar s -> formatGetVar s
  thisLam@(ELambda upvals args body) -> do
    lamName <- lift $ addNewLam thisLam
    cloName <- gensym "newClosure_"
    emitNewVar cloName
    allocClosure cloName lamName upvals (length args)
    return cloName
  _ -> return $ genAtom expr

genAtom :: Expr -> String
genAtom expr = case expr of
  EInt i -> "Scm_MkInt(" ++ show i ++ ")"
  EBool b -> if b then "Scm_True" else "Scm_False"
  EUnbound -> "Scm_Unbound"
  EUnspecified -> "Scm_Unspecified"
  ENil -> "Scm_Nil"
  _ -> error $ "genAtom: illegal expr: " ++ show expr

genStmt :: Expr -> GenCFunc ()
genStmt expr = case expr of
  EDefine name form -> do
    emitNewVar name -- XXX
    rhs <- genExpr form
    emit $ name ++ " = " ++ rhs ++ ";"
  ESete name form -> do
    rhs <- genExpr form
    setStmt <- formatSetVar name rhs
    emitStmt setStmt
  _ -> error $ "genStmt: illegal stmt: " ++ show expr

allocClosure :: String -> String -> [String] -> Int -> GenCFunc ()
allocClosure cloName lamName upvals argc = do
  let extraSize = 8 * length upvals
  -- inlined alloc
  emit $ cloName ++ " = " ++ formatAlloc ("sizeof(ScmClosure) + " ++
         show extraSize) ++ ";"
  emit $ "if (!" ++ cloName ++ ") {"
  -- if alloc failed
  extraLocals <- liftM cfExtraLocals get
  funcLocals <- liftM (extraLocals++) findLocals
  emit $ "Scm_PushGcRoot(" ++ curr_closure ++ ");"
  forM funcLocals $ \name -> do
    emit $ "Scm_PushGcRoot(" ++ name ++ ");"
  emit "Scm_GcCollect();"
  -- restore
  forM (reverse funcLocals) $ \name -> do
    emitStmt $ name ++ " = Scm_PopGcRoot()"
  emitStmt $ curr_closure ++ " = Scm_PopGcRoot()"
  emitStmt $ cloName ++ " = " ++ formatAlloc ("sizeof(ScmClosure) + " ++
             show extraSize)
  emitStmt $ formatAssert cloName
  -- end if
  emit "}"
  -- set type, put funcptr, upvals, name and arity
  emit $ formatObjType cloName ++ " = Scm_ClosureType;"
  emit $ formatClosureCode cloName ++ " = " ++ lamName ++ ";"
  forM_ (zip [0..] upvals) $ \(i, name) -> do
    fromVar <- formatGetVar name
    emitStmt $ formatSetUpval cloName i fromVar
  emitStmt $ formatClosureName cloName ++ " = " ++ show lamName
  emitStmt $ formatClosureArity cloName ++ " = " ++ show argc
  addExtraLocal cloName -- Hack

formatAlloc :: String -> String
formatAlloc size = "Scm_GcAlloc(" ++ size ++ ")"

formatAssert :: String -> String
formatAssert name = "Scm_Assert(" ++ name ++ ")"

formatGetUpval :: String -> Int -> String
formatGetUpval cloName i
  = "ScmClosure_GetUpvalAt(" ++ comma_join [cloName, show i] ++ ")"

formatSetUpval :: String -> Int -> String -> String
formatSetUpval cloName i value
  = "ScmClosure_SetUpvalAt(" ++ comma_join [cloName, show i, value] ++ ")"

formatClosureCode :: String -> String
formatClosureCode cloName = "ScmClosure_Code(" ++ cloName ++ ")"

formatClosureName :: String -> String
formatClosureName cloName = "ScmClosure_Name(" ++ cloName ++ ")"

formatClosureArity :: String -> String
formatClosureArity cloName = "ScmClosure_Arity(" ++ cloName ++ ")"

formatObjType :: String -> String
formatObjType obName = "Scm_HeapObjType(" ++ obName ++ ")"

formatGetVar :: String -> GenCFunc String
formatGetVar name = do
  varMap <- liftM cfVars get
  return $ case Map.lookup name varMap of
    Just (Local s) -> s
    Just (Upval i) -> formatGetUpval curr_closure i
    Nothing -> name

-- var = expr
formatSetVar :: String -> String -> GenCFunc String
formatSetVar vName eName = do
  varMap <- liftM cfVars get
  case Map.lookup vName varMap of
    Just (Local s) ->
      return $ "Scm_SetLocal(" ++ comma_join [vName, eName] ++ ")"
    Just (Upval i) ->
      return $ formatSetUpval curr_closure i eName
    Nothing ->
      return $ "Scm_SetGlobal(" ++ comma_join [vName, eName] ++ ")"

findLocals' :: CFunc -> [String]
findLocals' = map unLocal . filter isLocal . Map.elems . cfVars
  where
    isLocal (Local _) = True
    isLocal _ = False
    unLocal (Local s) = s

findLocals :: GenCFunc [String]
findLocals = liftM findLocals' get

