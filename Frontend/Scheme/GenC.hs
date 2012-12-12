module Frontend.Scheme.GenC (
  runGenC,
  genExpr
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.Scheme.AST
import Util.Temp

data GenState
  = GenState {
  }

