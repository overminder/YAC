module Frontend.Scheme.Normalize (
  runNormalGen,
  normalizeProgram
) where

import Frontend.ObjModel
import Util.Temp

data NormalState = NormalState {
  toplevelInitializers :: [(String, Cell)]
}

type NormalGen = StateT NormalState TempGen

runNormalGen :: NormalGen a -> TempGen a
runNormalGen = id

normalizeProgram :: [Cell] -> CPSTransGen [Cell]
normalizeProgram cs = do
  mapM_ normalizeToplevel cs
  initPairs <- liftM toplevelInitializers get
  <- forM inits $ \(name, form) ->
    x

