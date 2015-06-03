----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.ClientProxyCpp where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Language
import CppHelper
import Helper

----------------------------------------------------------------------------------------------------

renderClientProxy :: Module -> Interface -> T.Text
renderClientProxy modul interface = [st|
#include "#{modName modul}.h"

#include "dbserver/CinemaDispatch.h"

namespace
{

class #{infcName interface}Proxy : public I#{infcName interface}
{
public:
#{infcName interface}Proxy(CINEMA::ObjectDispatcher_ap disp, int dispId)
   : _disp(disp), _dispId(dispId)
{
}

#{methods}

private:
   CINEMA::ObjectDispatcher_ap _disp;
   int _dispId;

}; // class #{infcName interface}Proxy

} // namespace

|]
   where
      methods = T.concat $ map renderProxyMethod $ infcMethods interface


renderProxyMethod :: Method -> T.Text
renderProxyMethod method = [st|
virtual #{retType} #{name}(#{params})
{
   CINEMA::AttrObject params(_processMonitorId, CINEMA::to_attr(#{renderMethodId method}));
#{renderAssignParams method}

   _disp->call(params);
#{renderProxyMethodReturn method}
}
|]
   where
      name = metName method
      retType = renderType $ metRetType method
      params = T.intercalate ", " $ map renderParam $ metParams method


renderProxyMethodReturn :: Method -> T.Text
renderProxyMethodReturn method = if isVoid retType
   then T.empty
   else [st|   return params[P_RETURN].#{attrToTypeFunction retType};|]
      where
         retType = metRetType method


renderAssignParams :: Method -> T.Text
renderAssignParams method = unlinesIndent 3 $ map renderAssignParam $ zip [1..] $ metParams method


renderAssignParam :: (Int, (Type, T.Text)) -> T.Text
renderAssignParam (pos, (typ, name)) = if isCustomType typ
   then [st|
   params[P_#{pos}] = new CINEMA::AttrObject;
   #{name}.marshal(params[P_#{pos}].Object());
|]

   else [st|params[P_#{pos}] = #{name};|]

----------------------------------------------------------------------------------------------------
