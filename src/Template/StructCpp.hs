
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
   #{name}(CINEMA::AttrObject& obj);
   void marshal(CINEMA::AttrObject& obj) const;
#{getMethods}
private:
#{unlines' 3 attrDecls}
}; // class #{name}

|]
   where
      name = stName struct
      attrDecls = map renderAttrDecl $ stAttrs struct
      attrParams = renderParams $ map attrPair $ stAttrs struct
      getMethods = T.unlines $ map renderGetMethod $ stAttrs struct


renderStructImpl :: Struct -> T.Text
renderStructImpl struct = [st|
#{name}::#{name}(#{attrParams})
   : #{membersInit}
{
}
|]
   where
      name = stName struct
      attrParams = renderParams $ map attrPair $ stAttrs struct
      membersInit = renderMembersInit $ stAttrs struct


renderMemberInit :: Attr -> T.Text
renderMemberInit (Attr _ n) = [st|_#{n}(#{n})|]


renderMembersInit :: [Attr] -> T.Text
renderMembersInit attrs = T.intercalate ", " $ map renderMemberInit attrs


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


attrPair :: Attr -> (Type, T.Text)
attrPair (Attr t n) = (t, n)

----------------------------------------------------------------------------------------------------


