module Util.Ppr (
  Ppr(..),
  ppr,
  write,
  writeLn,
  newLine,
  indent
) where

import Control.Monad.State

type Render = State RenderState

class Ppr a where
  render :: a -> Render ()

data RenderState
  = RenderState {
    rsIndent :: Int,
    rsContent :: String,
    rsLineEmpty :: Bool
  }

empty = RenderState 0 [] True

ppr :: Render () -> String
ppr m = rsContent $ execState m empty

setLineEmpty :: Bool -> Render ()
setLineEmpty b = modify $ \st -> st {
  rsLineEmpty = b
}

indent :: Int -> Render ()
indent i = modify $ \st -> st {
  rsIndent = rsIndent st + i
}

-- writer monad?
write' :: String -> Render ()
write' s = modify $ \st -> st {
  rsContent = rsContent st ++ s
}

write :: String -> Render ()
write s = do
  lineEmpty <- liftM rsLineEmpty get
  if lineEmpty
    then do
      setLineEmpty False
      iLevel <- liftM rsIndent get
      write' $ take iLevel (repeat ' ')
    else
      pass
  write' s

pass = return ()

writeLn :: String -> Render ()
writeLn s = do
  write s
  newLine

newLine :: Render ()
newLine = do
  write "\n"
  setLineEmpty True

