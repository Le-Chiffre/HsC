module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

int :: Parser Expr
int = do
  n <- integer
  return $ Integral n

floating :: Parser Expr
floating = do
  n <- float
  return $ Floating n

expr :: Parser Expr
expr = Ex.buildExpressionParser [] factor

variable :: Parser Expr
variable = do
  var <- varID
  return $ Var var

call :: Parser Expr
call = do
  name <- varID
  args <- many expr
  return $ Call name args

stmts :: Parser Expr
stmts = do
  stmt <- expr
  newline
  stmts
  
factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> variable
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
  
simpletype :: Parser SimpleType
simpletype = do name <- conID
                args <- many varID
                return (name, args) 
  
btype :: Parser Type
btype = do t <- many1 gtype 
           case t of
            (t:[]) -> return t
            (t:ts) -> return $ TType t ts
  
gtype :: Parser Type
gtype =  try (string "()" >> return Unit)
     <|> try (brackets ptype >>= \a -> return $ List a)
     <|> try (parens $ commaSep ptype >>= \a -> return $ Tuple a)
     <|> try (varID >>= \a -> return $ TyVar a) 
     <|> try (conID >>= \a -> return $ TyCon a)
     <|> try (parens ptype)
  
ptype :: Parser Type
ptype = do (t:ts) <- arrowSep1 btype
           return $ foldl FType t ts

passign :: Parser Expr
passign = reservedOp "=" >> expr
  
type VarGen a = VarID -> Maybe Type -> Parser a
  
vdecl :: Parser Type -> VarGen a -> VarGen a -> Parser a
vdecl typ fa fna = do 
    name <- varID
    (do ty <- trytype; fa name (Just ty)) <|> fna name Nothing
 where trytype = try (colon >> typ)
       
constantdecl :: Parser Decl
constantdecl = vdecl ptype assign assign
 where assign n ty = passign >>= \v -> return $ ConstantDecl n ty v
                    
var :: Parser Type -> Bool -> Parser Var
var typ n = vdecl typ (if n then assign else noassign) noassign
 where noassign n ty = do v <- passign; return (n, ty, (Just v))
       assign n ty = do v <- optionMaybe (try passign); return (n, ty, v)
       
vardecl :: Parser Decl
vardecl = do reservedOp "var" 
             f <- var ptype True
             return $ VarDecl f

fundecl :: Parser Decl
fundecl = do name <- varID
             args <- many (var btype False)
             arrowr
             body <- expr
             return $ FunDecl name args body

tdecl :: String -> (SimpleType -> Type -> Decl) -> Parser Decl
tdecl n d = do reserved n
               name <- simpletype
               colon
               target <- ptype
               return $ d name target
              
typedecl = tdecl "type" TypeDecl
newtypedecl = tdecl "newtype" NewtypeDecl

datadecl :: Parser Decl
datadecl = do reserved "data"
              name <- simpletype
              arrowr
              decls <- many decl
              return $ DataDecl name decls
                   
classdecl :: Parser Decl
classdecl = do reserved "class"
               name <- simpletype
               base <- basedecl
               arrowr
               decls <- many decl
               return $ ClassDecl name base decls 
 where basedecl :: Parser SimpleType
       basedecl = try d <|> return ("Object", [])
        where d = colon >> simpletype
               
--variantdecl :: Parser Decl
--variantdecl = do reserved "variant"
--                 name <- simpletype
                 

decl :: Parser Decl
decl = typedecl
    <|> newtypedecl
    <|> datadecl
    <|> classdecl
    <|> vardecl
    <|> try fundecl
    <|> try constantdecl

toplevel :: Parser [Decl]
toplevel = many $ do
    def <- decl
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Decl]
parseToplevel s = parse (contents toplevel) "<stdin>" s