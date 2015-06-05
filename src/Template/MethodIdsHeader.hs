
----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.MethodIdsHeader(renderMethodIdsHeader) where

import Text.Shakespeare.Text
import qualified Data.Text as T

import Language
import CppHelper
import Helper

----------------------------------------------------------------------------------------------------

renderMethodIdsHeader :: FilePath -> Module -> T.Text
renderMethodIdsHeader fileBaseName modul = [st|
#ifndef #{headerDef}
#define #{headerDef}

#{renderBeginNs ns}
#{content}
#{renderEndNs ns}

#endif // #{headerDef}
|]
   where
      ns = modNs modul
      headerDef = headerDefine fileBaseName ns
      content = T.unlines . map renderMethodIdsClass $ modInterfaces modul


renderMethodIdsClass :: Interface -> T.Text
renderMethodIdsClass interface = [st|
struct #{infcName interface}MethodIds
{
   enum MethodId
   {
#{methodAssigns}
   };
};
|]
   where
      methodAssigns = unlinesIndent 6 . map renderMethodIdAssign . zip [100..] $ infcMethods interface


renderMethodIdAssign :: (Int, Method) -> T.Text
renderMethodIdAssign (methodId, name) = [st|#{renderMethodId name} = #{methodId},|]


----------------------------------------------------------------------------------------------------
