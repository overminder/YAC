module Backend.IR.Temp (
  TempGen,
  nextTemp,
  runTempGen
) where

import Control.Monad.State

type TempGen = State Int

nextTemp :: TempGen Int
nextTemp = do
  i <- get
  put $ i + 1
  return i

runTempGen :: TempGen a -> a
runTempGen tg = evalState tg 0

