
----------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import CddParser
import CppGenerator

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   text <- TIO.readFile "Test/test0.cdd"
   let modul = parse "Test/test0.cdd" text
   genFiles "out" modul
   putStrLn "ready"

----------------------------------------------------------------------------------------------------
