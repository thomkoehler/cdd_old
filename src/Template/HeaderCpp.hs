----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.HeaderCpp(renderHeader) where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderHeader :: String -> Module -> T.Text -> T.Text
renderHeader fileBaseName modul content = [st|
#ifndef #{headerDef}
#define #{headerDef}

#include "boost/shared_ptr.hpp"

#{renderBeginNs ns}
#{content}
#{renderEndNs ns}

#endif // #{headerDef}
|]
   where
      ns = modNs modul
      headerDef = headerDefine fileBaseName ns

----------------------------------------------------------------------------------------------------

