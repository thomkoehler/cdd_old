
----------------------------------------------------------------------------------------------------

module Main where

import Template.StructCppHeader
import Language

import StringTemplateQQ

----------------------------------------------------------------------------------------------------

str0 = Struct
   {
      stName = "Class",
      stAttributes = []
   }


main = do
   putStrLn $ show $ showTemplate str0

----------------------------------------------------------------------------------------------------
