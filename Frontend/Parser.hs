module Frontend.Parser (
  readProg
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

import Frontend.ObjModel

symbol :: Parser Char
symbol = oneOf "!#$%&|+-*/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readProg :: String -> Cell
readProg input = case parse parseProg "Scheme" input of
  Left err -> listToCell [Symbol "parse-error", Symbol $ show err]
  Right val -> listToCell [Symbol "parse-success", val]

parseAtom :: Parser Cell
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Boolean True
    "#f" -> Boolean False
    _ -> Symbol atom

parseNumber :: Parser Cell
parseNumber = liftM (Fixnum . read) (many1 digit)

parsePair :: Parser Cell
parsePair = do
  char '('
  p <- try parseList <|> parseDottedPair
  char ')'
  return p

parseList :: Parser Cell
parseList = liftM listToCell $ sepBy parseCell spaces

parseDottedPair :: Parser Cell
parseDottedPair = do
  cars <- endBy parseCell spaces
  cdr <- do
    char '.'
    spaces
    parseCell
  return $ dottedListToCell cars cdr

parseQuoted :: Parser Cell
parseQuoted = do
  char '\''
  c <- parseCell
  return $ listToCell [Symbol "quote", c]

parseCell :: Parser Cell
parseCell = parseAtom
        <|> parseNumber
        <|> parsePair
        <|> parseQuoted

parseProg :: Parser Cell
parseProg = do
  cs <- endBy parseCell (many space)
  return $ listToCell cs

