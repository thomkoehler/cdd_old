----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.HeaderCpp(renderHeader) where

import Text.Shakespeare.Text
import qualified Data.Text as T
import Data.Char(toUpper)

import System.FilePath(takeBaseName)

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderHeader :: String -> Ns -> T.Text -> T.Text
renderHeader fileName ns@(Ns p) content = [st|
#ifndef #{headerDef}
#define #{headerDef}

#{renderBeginNs ns}

#{content}

#{renderEndNs ns}

#endif // #{headerDef}
|]
   where
      headerDef = T.map toUpper $ T.concat [T.intercalate "_" p, "_", T.pack (takeBaseName fileName)]

----------------------------------------------------------------------------------------------------

