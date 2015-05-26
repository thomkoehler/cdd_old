----------------------------------------------------------------------------------------------------

module CppGenerator(genFiles) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

import Language
import Template.HeaderCpp
import Template.StructCpp

----------------------------------------------------------------------------------------------------

--TODO genFiles :: FilePath -> Module -> IO ()
genFiles :: FilePath -> Module -> IO ()
genFiles baseDir modul = do
   genClientFiles baseDir modul


--TODO genClientFiles :: FilePath -> Module -> IO ()
genClientFiles :: FilePath -> Module -> IO ()
genClientFiles baseDir modul = do
   let baseFileName = T.unpack (modName modul) ++ "Client"
   let headerFileName = baseDir </> baseFileName ++ ".h"
   let structs = T.concat $ map renderStructDecl $ modStructDefs modul
   let header = renderHeader baseFileName modul structs

   putStrLn $ "Create file '" ++ headerFileName ++ "'..."
   TIO.writeFile headerFileName header

   let cppFileName = baseDir </> baseFileName ++ ".cpp"
   return ()


----------------------------------------------------------------------------------------------------