
---------------------------------------------------------------------------------------------------

module Helper(unlines') where


import qualified Data.Text as T

---------------------------------------------------------------------------------------------------

unlines' :: Int -> [T.Text] -> T.Text
unlines' n items =
   let
      indent = T.replicate n $ T.pack " "
      step = T.append indent
   in
      T.unlines $ map step items

---------------------------------------------------------------------------------------------------


