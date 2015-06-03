----------------------------------------------------------------------------------------------------

module CddParser(parse) where


import qualified Data.Text as T
import Text.Parsec.Pos
import Text.Parsec hiding(parse)
import Control.Monad(liftM)

import CddLexer
import Language

----------------------------------------------------------------------------------------------------

parse :: SourceName -> T.Text -> Module
parse srcName input = case runParser pModule () srcName input of
   Right res -> res
   Left err -> error $ show err


param :: IParser (Type, T.Text)
param = do
   t <- typ
   n <- identifier
   return (t, T.pack n)
   <?> "Param"


attrDecl :: IParser Attr
attrDecl = do
   (t, n) <- param
   _ <- symbol ";"
   return $ Attr t n
   <?> "Attr Decl"


typeAndVoid :: IParser Type
typeAndVoid = (reserved "Void" >> return TVoid) <|> typ <?> "Type"

typ :: IParser Type
typ = choice
   [
      reserved "Int" >> return TInt,
      reserved "Int64" >> return TInt64,
      reserved "String" >> return TString,
      reserved "Double" >> return TDouble,
      reserved "Bool" >> return TBool,
      reserved "Object" >> return TObject,
      reserved "GetFilter" >> return TGetFilter,
      reserved "CndFilter" >> return TCndFilter,
      complexType
   ]
   <?> "Type"


complexType :: IParser Type
complexType = choice
   [
--TODO      complexTypeWithNs,
      liftM (Type globalNs . T.pack) identifier
   ]

complexTypeWithNs :: IParser Type
complexTypeWithNs = do
   ns <- namespace
   symbol "."
   name <- identifier
   return $ Type ns $ T.pack name


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
   ns <- identifier `sepBy` symbol "."
   return $ Ns $ map T.pack ns


method :: IParser Method
method = do
   rt <- typeAndVoid
   name <- identifier
   params <- parens $ param `sepBy` symbol ","
   _ <- symbol ";"
   return $ Method (T.pack name) rt params


interface :: IParser Interface
interface = do
   spaces
   reserved "interface"
   name <- identifier
   methods <- braces $ many1 method
   return $ Interface (T.pack name) methods


pModule :: IParser Module
pModule = do
   spaces
   reserved "module"
   name <- identifier
   ns <- option globalNs (parens namespace)
   structs <- many struct
   interfaces <- many interface
   eof
   return $ Module (T.pack name) ns structs interfaces
   <?> "Modules"

----------------------------------------------------------------------------------------------------



