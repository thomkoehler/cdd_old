
---------------------------------------------------------------------------------------------------

module Helper
(
   unlines',
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

unlines' :: Int -> [T.Text] -> T.Text
unlines' n items =
   let
      step = T.append $ spaces n
   in
      T.unlines $ map step items


unlinesIntercalate :: Int -> T.Text -> [T.Text] -> T.Text
unlinesIntercalate _ _ [] = T.empty
unlinesIntercalate n _ [text] = spaces n `T.append` text
unlinesIntercalate n i ts = T.concat[unlines' n $ map fun (init ts), spaces n, last ts]
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
