----------------------------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Language where


import Data.Text

----------------------------------------------------------------------------------------------------

data Type
   = Type { tNs :: Ns, tName :: Text }
   | TVoid
   | TString
   | TInt
   | TInt64
   | TDouble
   | TBool
   | TObject
   | TGetFilter
   | TCndFilter
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


data Method = Method
   {
      metName :: Text,
      metRetType :: Type,
      metParams :: [(Type, Text)]
   }
   deriving Show

data Interface = Interface
   {
      infcName :: Text,
      infcMethods :: [Method]
   }
   deriving Show

data Module = Module
   {
      modName :: Text,
      modNs :: Ns,
      modStructDefs :: [Struct],
      modInterfaces :: [Interface]
   }
   deriving Show


isVoid :: Type -> Bool
isVoid TVoid = True
isVoid _ = False


isCustomType :: Type -> Bool
isCustomType Type {..} = True
isCustomType _ = False


isSimpleType :: Type -> Bool
isSimpleType Type {..} = False
isSimpleType TString = False
isSimpleType TObject = False
isSimpleType TGetFilter = False
isSimpleType TCndFilter = False
isSimpleType _ = True


isGlobalNs :: Ns -> Bool
isGlobalNs (Ns []) = True
isGlobalNs _ = False


globalNs :: Ns
globalNs = Ns []

----------------------------------------------------------------------------------------------------


