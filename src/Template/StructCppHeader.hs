
{-# LANGUAGE QuasiQuotes #-}

----------------------------------------------------------------------------------------------------

module Template.StructCppHeader(showTemplate) where


import StringTemplateQQ
import Text.StringTemplate
import Data.ByteString

import Language

----------------------------------------------------------------------------------------------------

showTemplate :: Struct String -> String
showTemplate struct =
   let
      vars = [("className", stName struct)]
   in
      toString [stemplate|

class $className$
{

}; class $className$

|]

----------------------------------------------------------------------------------------------------


