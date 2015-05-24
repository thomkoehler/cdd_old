
----------------------------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}

module StringTemplateQQ(stemplate) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Text.StringTemplate

----------------------------------------------------------------------------------------------------

stemplate :: QuasiQuoter
stemplate = QuasiQuoter
   {
      quoteExp = quoteTmplExp,
      quotePat = error "quotePat is not supported",
      quoteType = error "quoteType is not supported",
      quoteDec = error "quoteDec is not supported"
   }


quoteTmplExp :: String -> TH.ExpQ
quoteTmplExp s = return tmpl
  where
    base = TH.AppE (TH.VarE (TH.mkName "Text.StringTemplate.newSTMP")) (TH.LitE (TH.StringL s))
    tmpl = TH.AppE
      (TH.AppE
         (TH.VarE (TH.mkName "StringTemplateHelper.setManyAttribBox"))
         (TH.VarE (TH.mkName "vars")))
      base

----------------------------------------------------------------------------------------------------


