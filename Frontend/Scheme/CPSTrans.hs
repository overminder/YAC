module Frontend.Scheme.CPSTrans (
  runCPSTrans,
  transformToplevel
) where

import Control.Monad
import Control.Monad.Trans

import Frontend.Scheme.AST
import Util.Temp

type CPSTransGen = TempGen

runCPSTrans :: CPSTransGen a -> TempGen a
runCPSTrans = id

gensym :: String -> CPSTransGen String
gensym s = do
  i <- nextTemp
  return $ s ++ show i

-- Turn a toplevel lambda into cps form
transformToplevel :: Expr -> CPSTransGen Expr
transformToplevel (ELambda upvals args body) = do
  k <- gensym "$ScCont_"
  newBody <- contWith (EVar k) body
  return $ ELambda upvals (args ++ [k]) newBody

contWith :: Expr -> Expr -> CPSTransGen Expr
contWith k expr = case expr of
  EAp func args ->
    let funArgs = func:args
     in if all isAtom funArgs
          then do
            -- (fun arg1 ... argN) -> (fun arg1 ... argN k)
            return $ EAp func (args ++ [k])
          else do
            -- (fun (g arg) ... argN) ->
            -- (g arg (lambda ($ApCont_0)
            --          (fun $ApCont_0 ... argN k)))
            hole <- gensym "$ApCont_"
            let (fun':args', cExpr) = swapComplex funArgs (EVar hole)
            e' <- contWith k (EAp fun' args')
            contWith (ELambda [] [hole] e') cExpr
  EIf cond ifTrue ifFalse ->
    if isAtom cond
      then do
        -- (if atomCond (f x) y) ->
        -- (if atomCond (f x k) (k y))
        ifTrue' <- contWith k ifTrue
        ifFalse' <- contWith k ifFalse
        return $ EIf cond ifTrue' ifFalse'
      else do
        -- (if (f x) (g y) z ->
        -- (f x (lambda ($IfCont_0)
        --        (if $IfCont_0 (g y k) (k z))))
        hole <- gensym "$IfCont_"
        e' <- contWith k (EIf (EVar hole) ifTrue ifFalse)
        contWith (ELambda [] [hole] e') cond
  ESeq es ->
    case es of
      x:ys@(_:_) -> if isAtom x
        then do
          -- (begin
          --   var1
          --   (g x)) ->
          -- ((lambda (_)
          --    (g x k)) var1)
          hole <- gensym "$Unused_"
          rest <- contWith k (ESeq ys)
          return $ EAp (ELambda [] [hole] rest) [x]
        else do
          -- (begin
          --   (f x)
          --   (g x)) ->
          -- (f x (lambda (_)
          --    (g x k)))
          hole <- gensym "$Unused_"
          rest <- contWith k (ESeq ys)
          let newK = ELambda [] [hole] rest
          contWith newK x
      [x] -> contWith k x
      [] -> return $ EAp k [EUnspecified] -- Correct?
  EDefine name expr ->
    if isAtom expr
      then do
        -- (define a 1) -> (begin (define a 1) (k #<unspecified>))
        return $ ESeq [EDefine name expr, EAp k [EUnspecified]]
      else do
        -- (define a (f x)) ->
        -- (begin
        --   (define a #<unbound>)
        --   (f x (lambda ($Res)
        --          (begin
        --            (set! a $Res)
        --            (k #<unspecified>)))))
        hole <- gensym "$DefCont_"
        let newK = ELambda [] [hole] (ESeq [ESete name (EVar hole),
                                            EAp k [EUnspecified]])
        rest <- contWith newK expr
        return $ ESeq [EDefine name EUnbound, rest]
  ESete name expr ->
    if isAtom expr
      then do
        -- (set! a 1) -> (begin (set! a 1) (k #<unspecified>))
        return $ ESeq [ESete name expr, EAp k [EUnspecified]]
      else do
        -- (set! a (f x)) ->
        -- (f x (lambda ($Res)
        --        (begin
        --          (set! a $res)
        --          (k #<unspecified>))))
        hole <- gensym "$SetCont_"
        let newK = ELambda [] [hole] (ESeq [ESete name (EVar hole),
                                            EAp k [EUnspecified]])
        contWith newK expr
  ELambda upvals args body -> do
    -- (lambda (x y)
    --   (+ x y)) ->
    -- (k (lambda (x y $LamCont)
    --   (+ x y $LamCont)))
    lamK <- gensym "$LamCont_"
    newBody <- contWith (EVar lamK) body
    return $ EAp k [ELambda upvals (args ++ [lamK]) newBody]
  _ ->
    if isAtom expr
      -- Atoms
      -- v -> (k v)
      -- XXX: do beta reduction here?
      then
        return $ EAp k [expr]
      else
        error $ "contWith: Unknown expr: " ++ show expr

mergeESeq :: Expr -> Expr -> Expr
mergeESeq e1 e2 = merged
  where
    flat1 = flattenESeq e1
    flat2 = flattenESeq e2
    flat = flat1 ++ flat2
    merged = case flat of
      [] -> ESeq []
      [x] -> x
      _ -> ESeq flat

swapComplex :: [Expr] -> Expr -> ([Expr], Expr)
swapComplex orig hole = (lhs ++ (hole:rhs), x)
  where
    (lhs, x:rhs) = span isAtom orig

