
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
unlinesIntercalate n i [text] = T.concat [spaces n, text, i]

{--
unlinesIntercalate i n items =
   let
      indent = T.replicate n $ T.pack " "
      step = T.append indent
   in
      T.unlines $ map step items
--}

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


