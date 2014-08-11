
module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

reservedNames = ["var","if","else","for","in","while","break","continue",
                 "case","class","data","type","newtype","type","_"]

reservedOps = [":","=","'","=>","->","."]

isReserved name = scan reservedNames 
 where
  scan []     = False
  scan (r:rs) = case (compare r name) of
                 LT -> scan rs
                 EQ -> True
                 GT -> False

idHelper :: Parser Char -> String -> Parser String
idHelper first msg = lexeme $ try $
           do name <- helper first msg
              if isReserved name
               then unexpected ("reserved word " ++ show name)
               else return name
 where helper first msg = do c <- first
                             cs <- many alphaNum
                             return (c:cs)
                          <?> msg

symbol :: Parser Char
symbol = oneOf "!#$%&*+/<=>?@\\^|-~:."

conID = idHelper upper "ConID"
varID = idHelper (lower <|> char '_') "VarID"

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where style = emptyDef {
               Tok.commentLine = ";"
             , Tok.reservedOpNames = reservedOps
             , Tok.reservedNames = reservedNames
             , Tok.opLetter = symbol
             }

integer = Tok.integer lexer
float = Tok.float lexer
parens = Tok.parens lexer
brackets = Tok.brackets lexer
commaSep = Tok.commaSep lexer
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
lexeme = Tok.lexeme lexer

arrowSep1 p = sepBy1 p $ reservedOp "->"
arrowr = reservedOp "->"
colon = reservedOp ":"