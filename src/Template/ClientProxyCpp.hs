----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.ClientProxyCpp where

----------------------------------------------------------------------------------------------------

renderClientProxy :: Interface -> T.Text
renderClientProxy interface = [st|
#include"#{name}Proxy"
|]
   where
      name = infcName interface

----------------------------------------------------------------------------------------------------