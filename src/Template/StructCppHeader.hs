
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

----------------------------------------------------------------------------------------------------

module Template.StructCppHeader(genStruct) where


import Text.Shakespeare.Text
import Data.Text

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

genStruct :: Struct -> Text
genStruct struct = [st|

class #{name}
{
}
; // class #{name}

|]

   where
      name = stName struct

----------------------------------------------------------------------------------------------------


