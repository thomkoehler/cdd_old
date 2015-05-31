----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.ClientProxyCpp where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Language

----------------------------------------------------------------------------------------------------

renderClientProxy :: Module -> Interface -> T.Text
renderClientProxy modul interface = [st|
#include "#{modName modul}.h"

namespace
{

class #{infcName interface}Proxy : public I#{infcName interface}
{
public:
   #{infcName interface}Proxy()
   {
   }

};

} // namespace

|]


----------------------------------------------------------------------------------------------------
