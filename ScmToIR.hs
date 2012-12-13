import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad

import Util.Temp
import qualified Util.Ppr as Ppr
import Frontend.Parser
import Frontend.Scheme.AST
import Frontend.Scheme.Mangler
import Frontend.Scheme.CPSTrans
import Frontend.Scheme.UpvalLift
import Frontend.Scheme.GenC

pprScDefns scDefns =
  forM_ (Map.toList scDefns) $ \(name, expr) -> do
    pprScDefn name expr

pprScDefn name expr = do
  putStr $ name ++ " = "
  putStrLn $ Ppr.ppr $ Ppr.render expr
  putStrLn ""

pprCFuncDecl cf@(CFunc name vars code) = do
  let localVars = map ("ScmPtr "++) ("thisClosure":findLocals' cf)
  putStrLn $ "void " ++ name ++ "(" ++ intercalate ", " localVars ++ ");"

pprCFuncScInit cf@(CFunc name vars code) = do
  case name of
    'S':'c':'_':rest -> do
      putStrLn $ "static ScmClosure " ++ "_" ++ rest ++ " = " ++
                 "Scm_MkSuperComb(" ++ name ++ ");"
      putStrLn $ "ScmPtr " ++ rest ++ " = " ++
                 "(ScmPtr) &_" ++ rest ++ ";"
    _ -> return ()

pprCVarInit cf@(name, expr) = do
  case expr of
    EUnbound -> putStrLn $ "ScmPtr " ++ name ++ " = Scm_Unbound;"
    _ -> error $ "toplevel expr not allowed: " ++ show expr

pprCFunc cf@(CFunc name vars code) = do
  let localVars = map ("ScmPtr "++) ("thisClosure":findLocals' cf)
  putStrLn $ "void " ++ name ++ "(" ++ intercalate ", " localVars ++ ") {"
  mapM_ putStrLn code
  putStrLn "}"

main = do
  prog <- liftM readProgSucc getContents
  let (ast,cps,uvl,mgd,cfs,cvs) = runTempGen $ do
              defns <- runASTGen $ toAST prog
              cpsForm <- runCPSTrans $ forM (Map.toList defns) $
                                            \(name, expr) ->
                case expr of
                  ELambda _ _ _ -> do
                    expr' <- transformToplevel expr
                    return (name, expr')
                  _ ->
                    return (name, expr)
              uvLifted <- runUpvalGen $ forM cpsForm $ \(name, expr) ->
                case expr of
                  ELambda _ _ _ -> do
                    expr' <- liftUpval expr
                    return (name, expr')
                  _ ->
                    return (name, expr)
              mangled <- forM uvLifted $ \(name, expr) ->
                return (mangle name, fmap mangle expr)

              let lams = filter (\(_, e) -> isLambda e) mangled
                  cVars = filter (\(_, e) -> not $ isLambda e) mangled
                  isLambda (ELambda _ _ _) = True
                  isLambda _ = False
                  prefixedLams = map (\(name, e) -> ("Sc_" ++ name, e)) lams

              cFuncs <- runGenC (GenState [] prefixedLams) genToplevel
              return (defns
                     , Map.fromList cpsForm
                     , Map.fromList uvLifted
                     , Map.fromList mangled
                     , cFuncs
                     , cVars)
  putStrLn "/*"
  pprScDefns ast
  putStrLn (take 78 (repeat '*'))
  pprScDefns cps
  putStrLn (take 78 (repeat '*'))
  pprScDefns uvl
  putStrLn (take 78 (repeat '*'))
  pprScDefns mgd
  putStrLn (take 78 (repeat '*'))
  putStrLn "*/"
  putStrLn "#include \"scm_runtime.h\""
  mapM_ pprCFuncDecl cfs
  putStrLn ""
  mapM_ pprCFuncScInit cfs
  putStrLn ""
  mapM_ pprCVarInit cvs
  putStrLn ""
  mapM_ pprCFunc cfs
  return ()

