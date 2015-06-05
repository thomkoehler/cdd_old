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

using namespace #{renderNs (modNs modul)};

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

#{renderConnectMethod interface}

|]
   where
      methods = T.concat . map (renderProxyMethod interface) $ infcMethods interface


renderProxyMethod :: Interface -> Method -> T.Text
renderProxyMethod interface method = [st|
virtual #{retType} #{name}(#{params})
{
   CINEMA::AttrObject params(_dispId, CINEMA::to_attr(#{infcName interface}MethodIds::#{renderMethodId method}));
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
renderProxyMethodReturn method
   | isVoid retType = T.empty
   | isCustomType retType = [st|   return #{renderType retType}(params[P_RETURN].Object());|]
   | otherwise = [st|   return params[P_RETURN].#{attrToTypeFunction retType};|]
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


renderConnectMethod :: Interface -> T.Text
renderConnectMethod interface = [st|
#{name}_ap I#{name}::connect(const char* host, unsigned short port, int timeout)
{
   CINEMA::ObjectDispatcher_ap disp(ObjectDispatcher::create());
   disp->connect(host, port);
   CINEMA::OBJECT_TYPE id = disp->resolve(DISPATCHER_NAME);

   if(timeout >= 0)
   {
      disp->set_timeout(id, timeout);
   }

   return #{name}_ap(new #{name}Proxy(disp, id));
}
|]
   where
      name = infcName interface


----------------------------------------------------------------------------------------------------
