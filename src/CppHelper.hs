
---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module CppHelper where

import Text.Shakespeare.Text
import Data.Text

import Language

---------------------------------------------------------------------------------------------------

renderType :: Type -> Text
renderType TString = "std::string"
renderType TInt = "int"
renderType TInt64 = "_int64"


renderAttributeDecl :: Attribute -> Text
renderAttributeDecl (Attribute t n) = [st|#{renderType t} _#{n};|]

---------------------------------------------------------------------------------------------------



