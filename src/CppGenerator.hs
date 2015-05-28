----------------------------------------------------------------------------------------------------

module CppGenerator(genFiles) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

import Language
import Template.HeaderCpp
import Template.StructCpp
import Template.ClientInterfaceCpp
import Template.CppCpp

----------------------------------------------------------------------------------------------------

--TODO genFiles :: FilePath -> Module -> IO ()
genFiles :: FilePath -> Module -> IO ()
genFiles baseDir modul = do
   genClientFiles baseDir modul


--TODO genClientFiles :: FilePath -> Module -> IO ()
genClientFiles :: FilePath -> Module -> IO ()
genClientFiles baseDir modul = do
   let
      baseFileName = T.unpack (modName modul) ++ "Client"
      headerFileName = baseDir </> baseFileName ++ ".h"
      structs = T.concat $ map renderStructDecl $ modStructDefs modul
      interfaces = T.concat $ map renderClientInterface $ modInterfaces modul
      content = structs `T.append` interfaces
      header = renderHeader baseFileName modul content

   putStrLn $ "Create file '" ++ headerFileName ++ "' ..."
   TIO.writeFile headerFileName header

   let
      cppFileName = baseDir </> baseFileName ++ ".cpp"
      structs = T.concat $ map renderStructImpl $ modStructDefs modul
      cpp = renderCpp baseFileName modul structs

   putStrLn $ "Create file '" ++ cppFileName ++ "' ..."
   TIO.writeFile cppFileName cpp

   return ()


----------------------------------------------------------------------------------------------------