
---------------------------------------------------------------------------------------------------

module Helper
(
   unlinesIndent,
   unlinesIndent',
   camelCaseToUnderscore,
   camelCaseToUpperUnderscore,
   unlinesIntercalate
)
where


import qualified Data.Text as T
import Data.Char(isUpper, toUpper)

---------------------------------------------------------------------------------------------------

spaces :: Int -> T.Text
spaces n = T.replicate n $ T.pack " "

unlinesIndent :: Int -> [T.Text] -> T.Text
unlinesIndent n items =
   let
      makeIndent = T.append $ spaces n
   in
      T.stripEnd . T.unlines . map makeIndent . map T.strip $ items


unlinesIndent' :: Int -> [T.Text] -> T.Text
unlinesIndent' n items =
   let
      makeIndent = T.append $ spaces n
   in
      T.stripEnd . T.unlines . map makeIndent $ items


unlinesIntercalate :: Int -> T.Text -> [T.Text] -> T.Text
unlinesIntercalate _ _ [] = T.empty
unlinesIntercalate n _ [text] = spaces n `T.append` text
unlinesIntercalate n i ts = T.concat[unlinesIndent n $ map fun (init ts), spaces n, last ts]
   where
      fun txt = txt `T.append` i


camelCaseToUnderscore :: T.Text -> T.Text
camelCaseToUnderscore = T.concatMap step
   where
      step char = if isUpper char
         then underscore `T.append` T.singleton char
         else T.singleton char


camelCaseToUpperUnderscore :: T.Text -> T.Text
camelCaseToUpperUnderscore = T.map toUpper . camelCaseToUnderscore


underscore :: T.Text
underscore = T.pack "_"

---------------------------------------------------------------------------------------------------
