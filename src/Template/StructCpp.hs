
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

private:
#{unlines' 3 attrDecls}
};
|]
   where
      name = stName struct
      attrDecls = map renderAttrDecl $ stAttrs struct
      attrParams = renderAttrParams $ stAttrs struct


renderStructImpl :: Struct -> T.Text
renderStructImpl struct = [st|
#{name}::#{name}(#{attrParams})
   : #{membersInit}
{
}
|]
   where
      name = stName struct
      attrParams = renderAttrParams $ stAttrs struct
      membersInit = renderMembersInit $ stAttrs struct


renderMemberInit :: Attr -> T.Text
renderMemberInit (Attr _ n) = [st|_#{n}(#{n})|]


renderMembersInit :: [Attr] -> T.Text
renderMembersInit attrs = T.intercalate ", " $ map renderMemberInit attrs

----------------------------------------------------------------------------------------------------


