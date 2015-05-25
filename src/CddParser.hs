----------------------------------------------------------------------------------------------------

module CddParser(parse) where


import qualified Data.Text as T
import Text.Parsec.Pos
import Text.Parsec hiding(parse)
import Text.ParserCombinators.Parsec.Combinator

import CddLexer
import Language

----------------------------------------------------------------------------------------------------

parse :: SourceName -> T.Text -> Struct
parse srcName input = case runParser struct () srcName input of
   Right res -> res
   Left err -> error $ show err


attrDecl :: IParser Attr
attrDecl = do
   t <- simpleType
   n <- identifier
   return $ Attr t (T.pack(n))
   <?> "Attr Decl"


simpleType :: IParser Type
simpleType = choice
   [
      reserved "int" >> return TInt,
      reserved "int65" >> return TInt64,
      reserved "string" >> return TString,
      reserved "double" >> return TDouble
   ]
   <?> "Simple Type"


struct :: IParser Struct
struct = do
   reserved "struct"
   name <- identifier
   return $ Struct (T.pack(name)) []
   <?> "Struct"

----------------------------------------------------------------------------------------------------



