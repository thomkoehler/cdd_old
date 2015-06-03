
----------------------------------------------------------------------------------------------------

module CddLexer
(
   IParser,
   reserved,
   symbol,
   parens,
   braces,
   identifier
)
where


import qualified Data.Text as T
import qualified Text.Parsec.Token as P
import Text.Parsec
import Text.Parsec.Text()
import Text.Parsec.Language
import Control.Monad.Identity

----------------------------------------------------------------------------------------------------

type IParser a = ParsecT T.Text () Identity a

languageDef :: GenLanguageDef T.Text st Identity
languageDef = P.LanguageDef
   {
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames =
         [
            "module",
            "struct",
            "iterface",
            "Int",
            "Int64",
            "String",
            "Double",
            "Bool",
            "Void",
            "Object",
            "GetFilter",
            "CndFilter"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf "",
      P.reservedOpNames = [],
      P.caseSensitive  = True
   }


lexer :: P.GenTokenParser T.Text () Identity
lexer = P.makeTokenParser languageDef

identifier :: IParser String
identifier = P.identifier lexer

reserved :: String -> IParser ()
reserved = P.reserved lexer

symbol :: String -> IParser String
symbol = P.symbol lexer

parens :: IParser a -> IParser a
parens = P.parens lexer

braces :: IParser a -> IParser a
braces = P.braces lexer

----------------------------------------------------------------------------------------------------

