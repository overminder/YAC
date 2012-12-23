module Frontend.Ca.Parser (
  parseString
) where

import Control.Monad
import Data.Functor.Identity
import qualified Data.List as List
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

import Frontend.Ca.AST

type CaSyn   = Program String
type CaTop   = ToplevelDef String
type DataSyn = Data String
type FuncSyn = Func String
type StmtSyn = Stmt String
type ExprSyn = Expr String

memClassNames :: [String]
memClassNames = words "i8 i16 i32 i64 u8 u16 u32 u64 f32 f64"

specialTypes :: [String]
specialTypes = ["gcptr"]

languageDef
  = emptyDef { T.commentStart    = "/*"
             , T.commentEnd      = "*/"
             , T.nestedComments  = True
             , T.commentLine     = "//"
             , T.identStart      = letter <|> char '_'
             , T.identLetter     = alphaNum <|> char '_'
             , T.reservedNames   = [ "if"
                                   , "else"
                                   , "while"
                                   , "return"
                                   , "jump"
                                   , "extern"
                                   , "register"
                                   ] ++ memClassNames
                                     ++ specialTypes
             , T.reservedOpNames = words ("+ - * / = < <= > >= " ++
                                          "== != && || ! % ~ & | ^ " ++
                                          "<< >>")
             , T.caseSensitive   = True
             }

lexer = T.makeTokenParser languageDef

-- Token defs
ident = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer
braces = T.braces lexer
brackets = T.brackets lexer
numLit = T.naturalOrFloat lexer
strLit = T.stringLiteral lexer
semi = T.semi lexer
comma = T.comma lexer
ws = T.whiteSpace lexer

-- Syntax defs
pProgram :: Parser CaSyn
pProgram = ws >> many pToplevel >>= (return . MkProgram)

pToplevel :: Parser CaTop
pToplevel = pFunction <|> pData <|> pScope

pFunction :: Parser CaTop
pFunction = do
  name <- ident
  args <- pArgDecls
  stmt <- pStmt
  return . FuncDef $ Func name args stmt

pData :: Parser CaTop
pData = do
  (ty, name) <- pTypedIdent
  reservedOp "="
  lit <- pLit
  semi
  return . DataDef $ LiteralData ty name lit

pScope :: Parser CaTop
pScope = pExtern <|> pExternReg

pExtern :: Parser CaTop
pExtern = do
  reserved "extern"
  name <- ident
  semi
  return . ScopeDef $ Extern name

pExternReg :: Parser CaTop
pExternReg = do
  reserved "register"
  name <- ident
  reg <- strLit
  semi
  return . ScopeDef $ ExternReg name reg

pArgDecls :: Parser [(StorageType, String)]
pArgDecls = do
  parens (sepBy pTypedIdent comma)

pTypedIdent :: Parser (StorageType, String)
pTypedIdent = do
  ty <- pType
  name <- ident
  return (ty, name)

pType :: Parser StorageType
pType = (pMemClass >>= return . flip MkStorage False)
    <|> (reserved "gcptr" >> return gcptr)

pStmtList :: Parser StmtSyn
pStmtList = do
  xs <- many1 pStmt
  return $ if length xs == 1 then head xs else SBlock xs

pStmt :: Parser StmtSyn
pStmt = braces pStmtList <|> pCompoundStmt

pCompoundStmt :: Parser StmtSyn
pCompoundStmt = pIfStmt
            <|> pWhileStmt
            <|> pReturnStmt
            <|> pJumpStmt
            <|> try pAssignStmt
            <|> pDeclStmt
            <|> pExprStmt

pIfStmt :: Parser StmtSyn
pIfStmt = do
  reserved "if"
  cond <- pExpr
  thenStmt <- pStmt
  elseOrPass <- pElse <|> pNothing
  return $ SIf cond thenStmt elseOrPass

pElse :: Parser StmtSyn
pElse = do
  reserved "else"
  body <- pStmt
  return body

pNothing :: Parser StmtSyn
pNothing = return $ SBlock []

pWhileStmt :: Parser StmtSyn
pWhileStmt = do
  reserved "while"
  cond <- pExpr
  body <- pStmt
  return $ SWhile cond body

pReturnStmt :: Parser StmtSyn
pReturnStmt = do
  reserved "return"
  e <- pExpr
  semi
  return $ SReturn e

pJumpStmt :: Parser StmtSyn
pJumpStmt = do
  reserved "jump"
  e <- pExpr
  semi
  return $ case e of
    ECall info func args ->
      SJump $ ECall (List.insert TailCall info)  func args
    _ ->
      SJump $ ECall [LocalJump] e []

pAssignStmt :: Parser StmtSyn
pAssignStmt = do
  lVal <- pExpr
  guard $ isLVal lVal
  reservedOp "="
  rVal <- pExpr
  semi
  return $ SAssign lVal rVal

pDeclStmt :: Parser StmtSyn
pDeclStmt = do
  (ty, name) <- pTypedIdent
  semi
  return $ SVarDecl ty name

pExprStmt :: Parser StmtSyn
pExprStmt = do
  e <- pExpr
  semi
  guard $ is_call e
  return $ SJump e
  where
    is_call (ECall _ _ _) = True
    is_call _ = False

pExpr :: Parser ExprSyn
pExpr = buildExpressionParser opList pCall

opList :: OperatorTable String () Identity ExprSyn
opList = -- Unary
         [ [Prefix (reservedOp "~"  >> return (EUnary  BNot))]
         , [Prefix (reservedOp "!"  >> return (EUnary  LNot))]
         , [Prefix (reservedOp "-"  >> return (EUnary  ANeg))]
         -- Bitwise and/or
         , [Infix  (reservedOp "&"  >> return (EBinary BAnd))  AssocLeft]
         , [Infix  (reservedOp "|"  >> return (EBinary BOr))   AssocLeft]
         , [Infix  (reservedOp "^"  >> return (EBinary BXor))  AssocLeft]
         -- Arith
         , [Infix  (reservedOp "%"  >> return (EBinary AMod))  AssocLeft]
         , [Infix ((reservedOp "*"  >> return (EBinary AMul))  <|>
                   (reservedOp "/"  >> return (EBinary ADiv))) AssocLeft]
         , [Infix ((reservedOp "+"  >> return (EBinary AAdd))  <|>
                   (reservedOp "-"  >> return (EBinary ASub))) AssocLeft]
         -- Bitwise shift
         , [Infix ((reservedOp "<<" >> return (EBinary BShl))  <|>
                   (reservedOp ">>" >> return (EBinary BShr))) AssocLeft]
         -- Relational
         , [Infix  (reservedOp "<"  >> return (EBinary RLt ))  AssocLeft]
         , [Infix  (reservedOp "<=" >> return (EBinary RLe ))  AssocLeft]
         , [Infix  (reservedOp ">"  >> return (EBinary RGt ))  AssocLeft]
         , [Infix  (reservedOp ">=" >> return (EBinary RGe ))  AssocLeft]
         , [Infix  (reservedOp "==" >> return (EBinary REq ))  AssocLeft]
         , [Infix  (reservedOp "!=" >> return (EBinary RNe ))  AssocLeft]
         -- Logical
         , [Infix  (reservedOp "&&" >> return (EBinary LAnd))  AssocLeft]
         , [Infix  (reservedOp "||" >> return (EBinary LOr ))  AssocLeft]
         ]

pCall :: Parser ExprSyn
pCall = do
  func <- pTerm
  argss <- many pArgs
  return $ foldl (ECall [CCall]) func argss

pArgs :: Parser [ExprSyn]
pArgs = parens $ sepBy pExpr comma

pTerm :: Parser ExprSyn
pTerm = parens pExpr
    <|> pMRef
    <|> liftM ELit pLit
    <|> liftM EVar ident

pLit :: Parser Lit
pLit = liftM mk_num numLit
   <|> liftM LStr strLit
   where
     mk_num num = case num of
       Left i -> LInt i
       Right d -> LFlo d

pMRef :: Parser ExprSyn
pMRef = do
  mcls <- pMemClass
  e <- brackets pExpr
  return $ EUnary (MRef mcls) e

pMemClass :: Parser MemClass
pMemClass = foldr1 (<|>) (map p_memclass memClassNames)
  where
    p_memclass s = do
      reserved s
      return $ read_memclass s
    read_memclass (prefix:width) = (prefix, read width)

parseString :: String -> CaSyn
parseString str = case parse pProgram "<Ca source>" str of
  Left e -> error $ show e
  Right r -> r

