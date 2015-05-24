
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
      quotePat = error "Not supported",
      quoteType = error "Not supported",
      quoteDec = error "Not supported"
   }


quoteTmplExp :: String -> TH.ExpQ
quoteTmplExp s = return tmpl
  where
    base = TH.AppE (TH.VarE (TH.mkName "Text.StringTemplate.newSTMP")) (TH.LitE (TH.StringL s))
    tmpl = TH.AppE
      (
         TH.AppE
         (
            TH.VarE (TH.mkName "Text.StringTemplate.setManyAttrib")
         )
         (
            TH.VarE (TH.mkName "vars")
         )
      )
      (
         base
      )

----------------------------------------------------------------------------------------------------


