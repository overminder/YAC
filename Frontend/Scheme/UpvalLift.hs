module Frontend.Scheme.UpvalLift (
  runUpvalGen,
  liftUpval
) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

import Frontend.Scheme.AST
import Util.Temp

type Env = Set String

data UpvalState
  = UpvalState {
    localVars :: Env,
    outerScope :: Maybe UpvalState,
    upvals :: Env
  }

empty = UpvalState Set.empty Nothing Set.empty

type UpvalGen = StateT UpvalState TempGen

runUpvalGen :: UpvalGen a -> TempGen a
runUpvalGen m = evalStateT m empty

withNewLambda :: UpvalGen a -> UpvalGen a
withNewLambda m = do
  st <- get
  put $ empty {outerScope = Just st}
  a <- m
  (Just outer) <- liftM outerScope get
  put outer
  return a

liftUpval :: Expr -> UpvalGen Expr
liftUpval expr = case expr of
  ELambda _ args body ->
    withNewLambda $ do
      mapM_ addLocal args
      body' <- liftUpval body
      us <- liftM (Set.toList . upvals) get
      return $ ELambda us args body'
  EVar name -> do
    tryAddUpval name
    return expr
  EAp func args -> do
    func' <- liftUpval func
    args' <- mapM liftUpval args
    return $ EAp func' args'
  EIf a b c -> do
    [a', b', c'] <- mapM liftUpval [a, b, c]
    return $ EIf a' b' c'
  ESeq xs -> do
    xs' <- mapM liftUpval xs
    return $ ESeq xs'
  EDefine name e -> do
    addLocal name
    e' <- liftUpval e
    return $ EDefine name e'
  ESete name e -> do
    tryAddUpval name
    e' <- liftUpval e
    return $ ESete name e'
  _ -> return expr

tryAddUpval :: String -> UpvalGen ()
tryAddUpval name = do
  st <- get
  let (_, st') = resolveName name st
  put st'

-- Return value: (Found, newState)
resolveName :: String -> UpvalState -> (Bool, UpvalState)
resolveName name st = case Set.member name (localVars st) of
  True -> (True, st)
  False -> case outerScope st of
    Nothing -> (False, st)
    Just outer -> let (found, outer') = resolveName name outer
                      inserter        = Set.insert name 
                   in if found
                        then
                          (True, st {
                            outerScope = Just outer',
                            localVars = inserter (localVars st),
                            upvals = inserter (upvals st)
                          })
                        else
                          (False, st)

addLocal :: String -> UpvalGen ()
addLocal name = modify $ \st -> st {
  localVars = Set.insert name (localVars st)
}

