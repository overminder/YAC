module Frontend.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

import Frontend.ObjModel

symbol :: Parser Char
symbol = oneOf "!#$%&|+-*/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readCell :: String -> String
readCell input = case parse parseCell "Scheme" input of
  Left err -> error "foo"
  Right val -> "Found " ++ show val

parseAtom :: Parser Cell
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Boolean True
    "#f" -> Boolean False
    otherwise -> Symbol atom

parseNumber :: Parser Cell
parseNumber = liftM (Fixnum . read) (many digit)

parseCell :: Parser Cell
parseCell = parseAtom <|> parseNumber
