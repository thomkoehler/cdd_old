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
   deriving Show


data Ns = Ns
   {
      nsPath :: [Text]
   }
   deriving Show


data Attr = Attr
   {
      attrType :: Type,
      attrName :: Text
   }
   deriving Show


data Struct = Struct
   {
      stName :: Text,
      stAttrs :: [Attr]
   }
   deriving Show


data Module = Module
   {
      modName :: Text,
      modNs :: Ns,
      modStructDefs :: [Struct]
   }
   deriving Show


isSimpleType :: Type -> Bool
isSimpleType Type {..} = False
isSimpleType TString = False
isSimpleType _ = True


isGlobalNs :: Ns -> Bool
isGlobalNs (Ns []) = True
isGlobalNs _ = False


globalNs :: Ns
globalNs = Ns []

----------------------------------------------------------------------------------------------------


