----------------------------------------------------------------------------------------------------

module Language where


import Data.Text

----------------------------------------------------------------------------------------------------


data Type = TString | TInt | TInt64


data Attribute = Attribute
   {
      attrType :: Type,
      attrName :: Text
   }


data Struct = Struct
   {
      stName :: Text,
      stAttributes :: [Attribute]
   }

----------------------------------------------------------------------------------------------------


