
----------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO(hPutStr)
import System.IO(stdout)

import Template.StructCpp
import Language
import CddParser

----------------------------------------------------------------------------------------------------

ns = Ns { nsPath = ["level0", "level1"]}

str0 = Struct
   {
      stName = "Class",
      stAttrs = [Attr TInt "intMember", Attr TString "strMember"]
   }


main = do
   hPutStr stdout $ renderStructDecl str0
   hPutStr stdout $ renderStructImpl str0


----------------------------------------------------------------------------------------------------
