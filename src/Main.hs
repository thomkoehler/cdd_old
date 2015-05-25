
----------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Template.StructCppHeader
import Language

----------------------------------------------------------------------------------------------------

str0 = Struct
   {
      stName = "Class",
      stAttributes = [Attribute TInt "intMember", Attribute TString "strMember"]
   }


main = do
   putStrLn $ show $ genStruct str0

----------------------------------------------------------------------------------------------------
