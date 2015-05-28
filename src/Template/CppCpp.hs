----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.CppCpp where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderCpp :: String -> Module -> T.Text -> T.Text
renderCpp fileBaseName modul content = [st|
#include "#{fileBaseName}.h"

using namespace #{renderNs ns};

#{content}
|]
   where
      ns = modNs modul

----------------------------------------------------------------------------------------------------
