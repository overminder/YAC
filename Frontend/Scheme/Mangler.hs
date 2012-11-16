module Frontend.Scheme.Mangler (
  mangle,
  demangle,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (ord, chr)

import qualified Frontend.Scheme.MangleTable as T

mangleMap :: Map Int String
mangleMap = Map.fromList T.table

demangleMap :: Map String Char
demangleMap = Map.fromList (map (\(x, y) -> (y, chr x)) $ Map.toList mangleMap)

mangle :: String -> String
mangle = concatMap mangleChar

mangleChar :: Char -> String
mangleChar c = mangleMap Map.! (ord c)

demangle :: String -> String
demangle s = case s of
  ('$':x:xs) -> (demangleMap Map.! ['$', x]):demangle xs
  (x:xs) -> x:demangle xs
  [] -> []

