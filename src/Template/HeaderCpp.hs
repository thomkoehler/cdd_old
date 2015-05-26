----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.HeaderCpp(renderHeader) where

import Text.Shakespeare.Text
import qualified Data.Text as T
import Data.Char(toUpper)

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderHeader :: String -> Module -> T.Text -> T.Text
renderHeader fileBaseName modul content = [st|
#ifndef #{headerDef}
#define #{headerDef}

#{renderBeginNs ns}
#{content}
#{renderEndNs ns}

#endif // #{headerDef}
|]
   where
      ns = modNs modul
      path =  nsPath ns
      headerDef = T.map toUpper $ T.concat [T.intercalate "_" path, "_", T.pack fileBaseName]

----------------------------------------------------------------------------------------------------

