
---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module CppHelper where

import Data.Text

import Language

---------------------------------------------------------------------------------------------------

showType :: Type -> Text
showType TString = "std::string"
showType TInt = "int"
showType TInt64 = "_int64"

showAttributeDecl :: Attribute -> Text
showAttributeDecl (Attribute t n) = showType t `append` " _" `append` n `append` ";"

---------------------------------------------------------------------------------------------------



