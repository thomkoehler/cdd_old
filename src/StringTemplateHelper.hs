
----------------------------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module StringTemplateHelper (AttrBox(..), setManyAttribBox) where

import Text.StringTemplate
--import Data.List(foldl')


----------------------------------------------------------------------------------------------------

data AttrBox = forall attr. ToSElem attr => AttrBox attr


setManyAttribBox attributes templates = foldl step templates attributes
   where
      step template (name, AttrBox attribute) = setAttribute name attribute template

----------------------------------------------------------------------------------------------------


