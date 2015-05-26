
----------------------------------------------------------------------------------------------------

module Template.HeaderCpp(renderHeader) where

import qualified Data.Text as T

----------------------------------------------------------------------------------------------------

renderHeader :: String -> Ns -> Text -> T.Text
renderHeader fileName (Ns p) content == [st|
#ifndef #{headerDef}
#define #{headerDef}
#{content}
#endif // #{headerDef}
|]
   where
      headerDef = T.map toUpper $ T.intercalate "_" p

----------------------------------------------------------------------------------------------------

