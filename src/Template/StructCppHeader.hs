
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

----------------------------------------------------------------------------------------------------

module Template.StructCppHeader(renderStructDecl) where


import Text.Shakespeare.Text
import qualified Data.Text as T

import Language
import CppHelper
import Helper

----------------------------------------------------------------------------------------------------

renderStructDecl :: Ns -> Struct -> T.Text
renderStructDecl ns struct = [st|

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

----------------------------------------------------------------------------------------------------


