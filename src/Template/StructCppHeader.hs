
{-# LANGUAGE QuasiQuotes #-}

----------------------------------------------------------------------------------------------------

module Template.StructCppHeader(showTemplate) where


import Text.StringTemplate.QQ
import Text.StringTemplate
import Data.ByteString

import Language

----------------------------------------------------------------------------------------------------

showTemplate :: Struct String -> String
showTemplate _ =
   let
      struct = Struct { stName = "Class", stAttributes = []}
      className =  stName struct
      test = "Test"
   in
      toString [stmp|

class $`className`$
{

}; // class $`className`$
}; // class $`className`$

|]

----------------------------------------------------------------------------------------------------


