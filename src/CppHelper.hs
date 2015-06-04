
---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module CppHelper
(
   renderType,
   renderAttrDecl,
   renderParam,
   renderParams,
   renderBeginNs,
   renderEndNs,
   renderNs,
   attrToTypeFunction,
   renderMethodId
) where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Language
import Helper

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
renderEndNs (Ns ps) = T.replicate (length ps) "}"

renderType :: Type -> T.Text
renderType TBool = "bool"
renderType TInt = "int"
renderType TInt64 = "__int64"
renderType TDouble = "double"
renderType TVoid = "void"
renderType TString = "std::string"
renderType TObject = "CINEMA::AttrObject"
renderType TGetFilter = "CINEMA::AttrGetFilter"
renderType TCndFilter = "CINEMA::AttrCndFilter"
renderType (Type ns name) = T.concat [renderNs ns, "::", name]


--TODO attrToTypeFunction :: Type -> T.Text
attrToTypeFunction :: Type -> T.Text
attrToTypeFunction TBool = "Bool()"
attrToTypeFunction TInt = "Int()"
attrToTypeFunction TInt64 = "Int64()"
attrToTypeFunction TDouble = "Double()"
attrToTypeFunction TString = "String()"
attrToTypeFunction TObject = "Object()"
attrToTypeFunction TGetFilter = "GetFilter()"
attrToTypeFunction TCndFilter = "CndFilter()"
attrToTypeFunction TVoid = "Cannot get a value from void."
attrToTypeFunction _ = error "AttrToTypeFunction not implemented yet."


renderAttrDecl :: Attr -> T.Text
renderAttrDecl (Attr t n) = [st|#{renderType t} _#{n};|]


renderParam :: (Type, T.Text) -> T.Text
renderParam (t, n) =
   if isSimpleType t
      then [st|#{renderType t} #{n}|]
      else [st|const #{renderType t} &#{n}|]


renderParams :: [(Type, T.Text)] -> T.Text
renderParams attrs = T.intercalate ", " $ map renderParam attrs


renderMethodId :: Method -> T.Text
renderMethodId method = "MID_" `T.append` camelCaseToUpperUnderscore (metName method)

---------------------------------------------------------------------------------------------------



