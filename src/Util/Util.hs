module Util.Util where

import XMonad

modMaskName :: KeyMask -> String
modMaskName modMaskUsed
  | modMaskUsed == mod1Mask = "alt"
  | modMaskUsed == mod2Mask = "Num_Lock"
  | modMaskUsed == mod3Mask = undefined
  | modMaskUsed == mod4Mask = "super"
  | modMaskUsed == mod5Mask = undefined
  | otherwise = undefined

(<|>) :: String -> String -> String
a <|> b = a <> " " <> b

