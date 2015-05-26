
---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module CppHelper
(
   renderType,
   renderAttrDecl,
   renderAttrParam,
   renderAttrParams,
   renderBeginNs,
   renderEndNs,
   renderNs
) where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Language

---------------------------------------------------------------------------------------------------

renderNs :: Ns -> T.Text
renderNs (Ns p) = T.intercalate "::" p


renderBeginNs :: Ns -> T.Text
renderBeginNs ns@(Ns ps) =
   let
      step p = T.concat ["namespace ", p, "{ "]
   in
      if isGlobalNs ns
         then T.empty
         else T.unlines $ map step ps


renderEndNs :: Ns -> T.Text
renderEndNs (Ns p) = T.replicate (length p) "}"


renderType :: Type -> T.Text
renderType TString = "std::string"
renderType TInt = "int"
renderType TInt64 = "__int64"
renderType TDouble = "double"
renderType (Type ns name) = renderNs ns `T.append` name


renderAttrDecl :: Attr -> T.Text
renderAttrDecl (Attr t n) = [st|#{renderType t} _#{n};|]


renderAttrParam :: Attr -> T.Text
renderAttrParam (Attr t n) =
   if isSimpleType t
      then [st|#{renderType t} #{n}|]
      else [st|const #{renderType t} &#{n}|]


renderAttrParams :: [Attr] -> T.Text
renderAttrParams attrs = T.intercalate ", " $ map renderAttrParam attrs

---------------------------------------------------------------------------------------------------



