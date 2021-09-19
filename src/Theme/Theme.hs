{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Module : Theme.Theme
--   Copyright : (c) 2021 Joan Milev <joantmilev@gmail.com>
--   License : MIT
--
--   Maintainer : Joan Milev <joantmilev@gmail.com>
--   Stability : Stable
--   Portability : Unknown
module Theme.Theme
  ( bgColor,
    fgColor,
    cursorColor,
    color00,
    color08,
    color01,
    color09,
    color02,
    color10,
    color03,
    color11,
    color04,
    color12,
    color05,
    color13,
    color06,
    color14,
    color07,
    color15,
    colorGrey,
    myFont,
    myFontGTK,
    myBigFont,
    myBoldFont,
    myItalicFont,
    myBarHeight,
  )
where

import Theme.Xresources (xprop)
import Prelude (String, Int)

colorGrey :: String
colorGrey = "#666666"

myBarHeight :: Int
myBarHeight = 18

bgColor,
  fgColor,
  cursorColor,
  color00,
  color08,
  color01,
  color09,
  color02,
  color10,
  color03,
  color11,
  color04,
  color12,
  color05,
  color13,
  color06,
  color14,
  color07,
  color15 :: String
cursorColor  = xprop "*.cursorColor"
bgColor      = xprop "*.background"
fgColor      = xprop "*.foreground"
color00      = xprop "*.color0"
color01      = xprop "*.color1"
color02      = xprop "*.color2"
color03      = xprop "*.color3"
color04      = xprop "*.color4"
color05      = xprop "*.color5"
color06      = xprop "*.color6"
color07      = xprop "*.color7"
color08      = xprop "*.color8"
color09      = xprop "*.color9"
color10      = xprop "*.color10"
color11      = xprop "*.color11"
color12      = xprop "*.color12"
color13      = xprop "*.color13"
color14      = xprop "*.color14"
color15      = xprop "*.color15"

myFont, myFontGTK, myBigFont, myBoldFont, myItalicFont :: String
myFont       = xprop "xmonad.font"
myFontGTK    = xprop "xmonad.font.gtk"
myBigFont    = xprop "xmonad.font.big"
myBoldFont   = xprop "xmonad.font.bold"
myItalicFont = xprop "xmonad.font.italic"
