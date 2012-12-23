import Frontend.Ca.AST
import Frontend.Ca.Parser

main = do
  src <- getContents
  putStrLn . show . parseString $ src
