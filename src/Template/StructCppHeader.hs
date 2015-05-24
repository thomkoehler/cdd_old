
{-# LANGUAGE QuasiQuotes #-}

----------------------------------------------------------------------------------------------------

module Template.StructCppHeader(showTemplate) where


import StringTemplateQQ
import Text.StringTemplate
import Data.ByteString

import Language
import StringTemplateHelper

----------------------------------------------------------------------------------------------------

showTemplate :: Struct String -> String
showTemplate struct =
   let
      vars =
         [
            ("name", AttrBox (stName struct)),
            ("attribs", AttrBox (stAttributes struct))
         ]
   in
      toString [stemplate|

class $name$
{

}; class $name$

|]

----------------------------------------------------------------------------------------------------


