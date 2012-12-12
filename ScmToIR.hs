import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

import Util.Temp
import qualified Util.Ppr as Ppr
import Frontend.Parser
import Frontend.Scheme.AST
import Frontend.Scheme.Mangler
import Frontend.Scheme.CPSTrans
import Frontend.Scheme.UpvalLift

pprScDefns scDefns =
  forM_ (Map.toList scDefns) $ \(name, expr) -> do
    pprScDefn name expr

pprScDefn name expr = do
  putStr $ name ++ " = "
  putStrLn $ Ppr.ppr $ Ppr.render expr
  putStrLn ""

main = do
  prog <- liftM readProgSucc getContents
  let (ast,cps,uvl,mgd) = runTempGen $ do
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
              return (defns
                     , Map.fromList cpsForm
                     , Map.fromList uvLifted
                     , Map.fromList mangled)
  pprScDefns ast
  putStrLn (take 78 (repeat '*'))
  pprScDefns cps
  putStrLn (take 78 (repeat '*'))
  pprScDefns uvl
  putStrLn (take 78 (repeat '*'))
  pprScDefns mgd
  return ()
