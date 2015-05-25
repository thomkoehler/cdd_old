
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

----------------------------------------------------------------------------------------------------

module Template.StructCppHeader(renderStructDecl) where


import Text.Shakespeare.Text
import Data.Text

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderStructDecl :: Struct -> Text
renderStructDecl struct = [st|

class #{name}
{
private:
}
; // class #{name}

|]

   where
      name = stName struct

----------------------------------------------------------------------------------------------------


