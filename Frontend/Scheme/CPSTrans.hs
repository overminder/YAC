module Frontend.Scheme.CPSTrans (
  runCPSTrans
)

import Util.Temp

type CPSTransGen = TempGen

runCPSTrans :: Cell -> Cell
runCPSTrans c = case c of
  (List cs) -> 
  _ -> error ""

