----------------------------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Language where


import Data.Text

----------------------------------------------------------------------------------------------------


data Type
   = Type { tNs :: Ns, tName :: Text }
   | TString
   | TInt
   | TInt64
   | TDouble


isSimpleType :: Type -> Bool
isSimpleType Type {..} = False
isSimpleType TString = False
isSimpleType _ = True


data Ns = Ns
   {
      nsPath :: [Text]
   }


isGlobalNs :: Ns -> Bool
isGlobalNs (Ns []) = True
isGlobalNs _ = False


data Attr = Attr
   {
      attrType :: Type,
      attrName :: Text
   }


data Struct = Struct
   {
      stName :: Text,
      stAttrs :: [Attr]
   }

----------------------------------------------------------------------------------------------------


