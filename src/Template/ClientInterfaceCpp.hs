----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.ClientInterfaceCpp(renderClientInterface) where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Helper
import CppHelper
import Language

----------------------------------------------------------------------------------------------------

renderClientInterface :: Interface -> T.Text
renderClientInterface interface = [st|
class I#{name};
typedef boost::shared_ptr<I#{name}> #{name}_ap;

class I#{name}
{
public:
   virtual I#{name}::~I#{name}(){}

#{unlinesIndent 3 methods}

   static #{name}_ap connect(const char* host, unsigned short port, int timeout);
}; // class I#{name}

|]
   where
      name = infcName interface
      methods = map renderInterfaceMethod $ infcMethods interface


renderInterfaceMethod :: Method -> T.Text
renderInterfaceMethod method = [st|virtual #{retType} #{name}(#{params}) = 0;|]
   where
      name = metName method
      retType = renderType $ metRetType method
      params = T.intercalate ", " $ map renderParam $ metParams method

----------------------------------------------------------------------------------------------------
