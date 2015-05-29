
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

----------------------------------------------------------------------------------------------------

module Template.StructCpp(renderStructDecl, renderStructImpl) where


import Text.Shakespeare.Text
import qualified Data.Text as T

import Language
import CppHelper
import Helper

----------------------------------------------------------------------------------------------------

renderStructDecl :: Struct -> T.Text
renderStructDecl struct = [st|
class #{name}
{
public:
   #{name}(#{attrParams});
   #{name}(CINEMA::AttrObject &obj);
   void marshal(CINEMA::AttrObject &obj) const;
#{getMethods}
private:
#{renderMarshallParameterIds struct}
#{unlines' 3 attrDecls}
}; // class #{name}

|]
   where
      name = stName struct
      attrDecls = map renderAttrDecl $ stAttrs struct
      attrParams = renderParams $ map attrPair $ stAttrs struct
      getMethods = T.unlines $ map renderGetMethod $ stAttrs struct


renderMarshallParameterIds :: Struct -> T.Text
renderMarshallParameterIds struct =
   unlines' 3 $ ["enum MarshalParameterId", "{"] ++ parameterIds ++ ["};"]
   where
      parameterIds = map renderMarshallParameterId (zip [1..] (stAttrs struct))


renderMarshallParameterId :: (Int, Attr) -> T.Text
renderMarshallParameterId (num, attr) = [st|   MP_#{camelCaseToUpperUnderscore name} = #{num},|]
   where
      name = attrName attr


renderStructImpl :: Struct -> T.Text
renderStructImpl struct = [st|
#{name}::#{name}(#{attrParams})
   : #{membersInit}
{
}
#{renderObjectConstr struct}
#{renderMarshalMethod struct}
|]
   where
      name = stName struct
      attrParams = renderParams $ map attrPair $ stAttrs struct
      membersInit = renderMembersInit $ stAttrs struct


renderGetMethod :: Attr -> T.Text
renderGetMethod attr = [st|
   #{typ} #{name}() const
   {
      return _#{name};
   }
|]
   where
      typ = renderType $ attrType attr
      name = attrName attr


renderMemberInit :: Attr -> T.Text
renderMemberInit (Attr _ n) = [st|_#{n}(#{n})|]


renderMembersInit :: [Attr] -> T.Text
renderMembersInit attrs = T.intercalate ", " $ map renderMemberInit attrs


renderObjectConstr :: Struct -> T.Text
renderObjectConstr struct = [st|
#{name}::#{name}(CINEMA::AttrObject &obj) :
#{objectConstrAttrs}
{
}
|]
   where
      name = stName struct
      objectConstrAttrs = unlinesIntercalate 3 "," $ map renderObjectConstrAttr $ stAttrs struct


renderObjectConstrAttr :: Attr -> T.Text
renderObjectConstrAttr attr =
   [st|_#{name}(obj[MP_#{camelCaseToUpperUnderscore name}].#{attrToTypeFunction t})|]
      where
         name = attrName attr
         t = attrType attr


renderMarshalMethod :: Struct -> T.Text
renderMarshalMethod struct = [st|
void #{structName}::marshal(CINEMA::AttrObject &obj) const
{
#{marshalAttrs}
}
|]
   where
      structName = stName struct
      marshalAttrs = unlines' 3 $ map renderMarshalAttr $ stAttrs struct


renderMarshalAttr :: Attr -> T.Text
renderMarshalAttr attr = [st|obj[MP_#{camelCaseToUpperUnderscore name}] = _#{name};|]
   where
      name = attrName attr


attrPair :: Attr -> (Type, T.Text)
attrPair (Attr t n) = (t, n)

----------------------------------------------------------------------------------------------------


