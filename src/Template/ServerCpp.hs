----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.ServerCpp where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Helper
import CppHelper
import Language

----------------------------------------------------------------------------------------------------

renderServerCpp :: Module -> Interface -> T.Text
renderServerCpp modul interface = [st|
#include "#{name}Server.h"

namespace
{

class #{name}MethodDispatcher : public CINEMA::MethodDispatcher<#{name}MethodDispatcher>
{
public:

#{name}MethodDispatcher(I#{name} *servant, CINEMA::ObjectDispatcher *disp)
   : _servant(servant), _disp(disp)
{
   __super::Parent(this);
#{registerMethods}
}


private:
#{methods}


   I#{name} *_servant;
   CINEMA::ObjectDispatcher *_disp;
};

} // namespace
|]
   where
      name = infcName interface
      registerMethods = unlinesIndent 3 $ map (renderRegisteMethod interface) $ infcMethods interface
      methods = unlinesIndent' 3 $ map renderMethod $ infcMethods interface


renderRegisteMethod :: Interface -> Method -> T.Text
renderRegisteMethod interface method =
   [st|__super::insert(CINEMA::to_attr(#{infcName interface}MethodIds::#{renderMethodId method}), &#{infcName interface}MethodDispatcher::#{metName method});|]


renderMethod :: Method -> T.Text
renderMethod method = [st|
CINEMA::Attribute_ap #{metName method}(CINEMA::AttrObject& obj)
{
   obj.clear();
   return CINEMA::to_attr(0);
}
|]

----------------------------------------------------------------------------------------------------

