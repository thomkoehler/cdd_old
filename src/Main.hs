
----------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO(hPutStr)
import System.IO(stdout)

import Template.StructCppHeader
import Language

----------------------------------------------------------------------------------------------------

str0 = Struct
   {
      stName = "Class",
      stAttributes = [Attribute TInt "intMember", Attribute TString "strMember"]
   }


main = hPutStr stdout $ renderStructDecl str0


----------------------------------------------------------------------------------------------------
