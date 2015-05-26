
----------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding(readFile)
import Data.Text.IO(hPutStr, readFile)
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
   text <- readFile "Test/test0.cdd"
   let str = parse "Test/test0.cdd" text
   hPutStr stdout $ renderStructDecl str
   hPutStr stdout $ renderStructImpl str


----------------------------------------------------------------------------------------------------
