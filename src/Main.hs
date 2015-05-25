
----------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO(hPutStr)
import System.IO(stdout)

import Template.StructCppHeader
import Language

----------------------------------------------------------------------------------------------------

ns = Ns { nsPath = ["level0", "level1"]}

str0 = Struct
   {
      stName = "Class",
      stAttrs = [Attr TInt "intMember", Attr TString "strMember"]
   }


main = hPutStr stdout $ renderStructDecl ns str0


----------------------------------------------------------------------------------------------------
