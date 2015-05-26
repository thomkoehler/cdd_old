----------------------------------------------------------------------------------------------------

module CddParser(parse) where


import qualified Data.Text as T
import Text.Parsec.Pos
import Text.Parsec hiding(parse)

import CddLexer
import Language

----------------------------------------------------------------------------------------------------

parse :: SourceName -> T.Text -> Module
parse srcName input = case runParser pModule () srcName input of
   Right res -> res
   Left err -> error $ show err


attrDecl :: IParser Attr
attrDecl = do
   t <- simpleType
   n <- identifier
   _ <- symbol ";"
   return $ Attr t $ T.pack n
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
   spaces
   reserved "struct"
   name <- identifier
   attrs <- braces $ many1 attrDecl
   return $ Struct (T.pack name) attrs
   <?> "Struct"


namespace :: IParser Ns
namespace = do
   ns <- identifier `sepBy` (symbol ".")
   return $ Ns $ map T.pack ns


pModule :: IParser Module
pModule = do
   spaces
   reserved "module"
   name <- identifier
   ns <- option globalNs (parens namespace)
   structs <- many struct
   eof
   return $ Module (T.pack name) ns structs
   <?> "Modules"

----------------------------------------------------------------------------------------------------



