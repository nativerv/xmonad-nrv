{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- XMonad
import XMonad
import qualified XMonad.StackSet as StackSet

-- Actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Promote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Actions.WithAll
import XMonad.Actions.Warp

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

-- Layout
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutHints

-- Config
import XMonad.Config.Desktop

-- Util
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Cursor
import XMonad.Util.NamedActions
import qualified XMonad.Util.Hacks as Hacks

-- Base
import qualified Data.Map as M
import Data.Monoid
import System.IO
import System.Exit

-- Theme
import Theme.Theme
import XMonad.Hooks.StatusBar.PP (xmobarFont)
import Data.List (isInfixOf)

import Common.Common
import Util.Util

main :: IO ()
main = do
  xmobarProc <- spawnPipe "killall xmobar; xmobar"

  xmonad
    $ ewmh
    $ withNavigation2DConfig myNav2DConfig
    $ addDescrKeys' ((myModMask .|. shiftMask, xK_slash), showKeybindings) myKeys
    $ Hacks.javaHack
    $ myConfig xmobarProc

    `removeKeysP`
    myEzRemovedKeybindings

myConfig xmobarProc =
    desktopConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , focusedBorderColor = myFocusedBorderColor
    , manageHook         = myManageHook
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , handleEventHook    = myHandleEventHook
    , logHook            = myLogHook xmobarProc
    , startupHook        = myStartupHook
    }

myLauncher :: String
myLauncher = "env sh -c dmenu_run"

myBorderWidth :: Dimension
myBorderWidth = 0

myInnerGapWidth :: Integer
myInnerGapWidth = 5

centerPointer :: X ()
centerPointer = warpToWindow 0.5 0.5

myNav2DConfig :: Navigation2DConfig
myNav2DConfig = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = lineNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)
    -- line/center same results   ,("Simple Tabs", lineNavigation)
    --                            ,("Simple Tabs", centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Simple Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Simple Tabs", fullScreenRect)
                                  ]
    }

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    zenityProc <- spawnPipe "zenity --text-info"
    hPutStr zenityProc (unlines $ showKm x)
    hClose zenityProc

xmobarLogHook :: Handle -> X ()
xmobarLogHook dest = dynamicLogWithPP xmobarPP
  { ppOutput           = hPutStrLn dest
  , ppVisible          = wsIconFont . wsNonemptyColor . wrap "(" ")"          -- % Visible with windows (on another monitor)
  , ppVisibleNoWindows = Just $ wsIconFont . wrap "(" ")"                     -- % Visible with no windows (on another monitor)
  , ppHidden           = wsIconFont . wsNonemptyColor                         -- % Hidden with no windows
  , ppCurrent          = wsIconFont . wsActiveUnderline .  wsNonemptyColor    -- % Visible (on current monitor)
  , ppHiddenNoWindows  = wsIconFont                                           -- % Not on any monitor and having no windows
  , ppUrgent           = wsUrgentColor                                        -- % Urgent (window on this tag wants your focus)
  , ppTitle            = titleColor
  , ppLayout           = layoutAction . layoutColor
  , ppSep              = xmobarColor colorGrey "" " | "
  }
  where
    wsIconFont         = xmobarFont 0
    --wsSurroundGapFont  = xmobarFont 6
    --wsActiveColor      = xmobarColor color10 ""
    wsActiveUnderline  = xmobarBorder "Bottom" color10 2
    wsNonemptyColor    = xmobarColor color01 ""
    wsUrgentColor      = xmobarColor "#e46e49" ""
    titleColor         = xmobarColor color01 ""
    layoutAction       = xmobarAction ("xdotool key " <> modMaskName myModMask <> "+space") "1"
    layoutColor        = xmobarColor color06 ""

myLogHook :: Handle -> X ()
myLogHook dest =
  mempty
  -- <+> centerPointer
  <+> xmobarLogHook dest
  -- <+> fadeInactiveLogHook'

myFocusedBorderColor :: String
myFocusedBorderColor = "#EEEEEE"

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . withGaps $ wsStrings
  where
    wsStrings  =
      -- globe     terminal  youtube   gamepad   wrench
      --[ "\64158", "\61728", "\61802", "\61723", "\57871", "hyd", "not" ]
        [ "www", ">_", "vid", "gam", "><", "wrk", "tool", "hyd", "gfx", "note" ]
    withGaps   = map (wrap smallSpace smallSpace)
    smallSpace = xmobarFont 6 " "
    escaped    = map xmobarEscape
    clickable workspaceList =
      [
        concat [ "<action=xdotool key "
                , modMaskName myModMask
                , "+"
                , show index
                , ">"
                , workspace
                , "</action>"
               ] |
        (index, workspace) <- zip [1..length workspaceList::Int] workspaceList
      ]

myInnerGapBorder :: Border
myInnerGapBorder = Border myInnerGapWidth myInnerGapWidth myInnerGapWidth myInnerGapWidth

myLayoutHook = tall ||| full
  where
    named n = renamed [XMonad.Layout.Renamed.Replace n]
    tall = named "[]="
      $ smartBorders
      $ avoidStruts
      $ spacingRaw False myInnerGapBorder True myInnerGapBorder True
      $ layoutHints
      --               MasterCount  ResizeAmount  MasterSize  SomeShit
      $ ResizableTall  1            (1/30)        0.5         []

    full = named "[ ]"
      $ smartBorders
      $ avoidStruts
      $ spacingRaw False myInnerGapBorder True myInnerGapBorder True
      $ layoutHints
      Full 

myHandleEventHook :: Event -> X All
myHandleEventHook = 
  mempty 
  <+> hintsEventHook
  <+> docksEventHook 
  <+> Hacks.windowedFullscreenFixEventHook

  -- % TODO: add this after updating xmonad
  -- % <+> Hacks.trayerPaddingXmobarEventHook
  <+> handleEventHook def

wsKeys :: [String]
wsKeys = map show $ [1..9::Int] ++ [0]

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf =
  let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    --screenKeys     = ["w","v","z"]
    dirKeys        = ["j","k","h","l"]
    dirs           = [ D,  U,  L,  R ]

    -- Zip with modKey
    zipM  modKey name actionKeys actions f = 
      zipWith (\key action -> (modKey ++ key, name $ f action)) actionKeys actions
    zipM' modKey name actionKeys actions f boolean = 
      zipWith (\key action -> (modKey ++ key, name $ f action boolean)) actionKeys actions

    incMaster = sendMessage (IncMasterN 1)
    decMaster = sendMessage (IncMasterN (-1))

    focusDown = windows StackSet.focusDown >> centerPointer
    focusUp = windows StackSet.focusUp >> centerPointer
    tile = withFocused $ windows . StackSet.sink
    fullFloat = withFocused 
      $ windows . flip StackSet.float (StackSet.RationalRect 0 0 1 1)
    cycleLayout = sendMessage NextLayout
    defaultLayout = setLayout (XMonad.layoutHook conf) 
  in

  subKeys "Window"
  (
  [   
      ("M-<Backspace>"      , addName "Kill This Copy Of Focused"      kill1)
    , ("M-S-<Backspace>"    , addName "Kill All Copies Of Focused"     kill)
    , ("M-M1-<Backspace>"   , addName "Kill All On Current Workspace"  kill)
    , ("M-b"                , addName "Promote To Master"              promote)
    , ("M-S-,"              , addName "Increment Master Size"          incMaster)
    , ("M-S-."              , addName "Decrement Master Size"          decMaster)
    , ("M-<Tab>"            , addName "Cycle Focus"                    focusDown)
    , ("M-S-<Tab>"          , addName "Cycle Focus Back"               focusUp)
    , ("M-t"                , addName "Tile Window"                    tile)
    , ("M-f"                , addName "FullFloat Window"               fullFloat)
  ]
                                                                                    -- Also warp cursor cursor when Navigating/Swapping - 
                                                                                    -- cause putting warp to logHook randomly warps w/o Navigating
                                                                                    -- when cursor is out of window client space
    ++ zipM' "M-"            (addName "Navigate ??? ??? ??? ???")              dirKeys dirs (\direction isWrap -> windowGo direction isWrap >> centerPointer) False
    ++ zipM' "M-C-"          (addName "Swap ??? ??? ??? ???")                  dirKeys dirs (\direction isWrap -> windowSwap direction isWrap >> centerPointer) False
    ++ zipM  "M-S-"          (addName "Move To Workspace #N")          wsKeys [0..] (withNthWorkspace StackSet.shift)
    ++ zipM  "M-C-"          (addName "Copy To Workspace #N")          wsKeys [0..] (withNthWorkspace copy)
  )
  ^++^

  subKeys "Workspace"
  (
  [
      ("M-i"                , addName "Cycle Left"                     prevWS) -- moveTo Prev AnyWS
    , ("M-o"                , addName "Cycle Right"                    nextWS) -- moveTo Next AnyWS
  ]
    ++ zipM "M-"             (addName "Switch to workspace")           wsKeys [0..] (withNthWorkspace StackSet.greedyView)
 )
  ^++^

  subKeys "Layout"
  [
      ("M-<Space>"          , addName "Cycle Layout"                   cycleLayout)
    , ("M-S-<Space>"        , addName "Set Layout To Default"          defaultLayout)
  ]
  ^++^

  subKeys "Spawn"
  [
      ("M-S-<Return>"       , addName "Terminal"                       $ spawn myTerminal)
    , ("M-p"                , addName "Launcher"                       $ spawn myLauncher)
  ]
   ^++^

  subKeys "Resize"
  [
      ("M-S-h"              , addName "Resize Left"                    $ sendMessage Shrink)
    , ("M-S-l"              , addName "Resize Right"                   $ sendMessage Expand)
    , ("M-S-j"              , addName "Resize Down"                    $ sendMessage MirrorShrink)
    , ("M-S-k"              , addName "Resize Up"                      $ sendMessage MirrorExpand)
  ]
  ^++^

  subKeys "Spacing"
  [
      ("M-["                , addName "Decrease Spacing"               $ incScreenWindowSpacing 1)
    , ("M-]"                , addName "Increase Spacing"               $ incScreenWindowSpacing (-1))
    , ("M-<Home>"           , addName "Reset Spacing"                  $ setScreenWindowSpacing myInnerGapWidth)

  ] ^++^

  subKeys "System"
  [
      ("M-S-q"              , addName "Quit XMonad"                    $ io exitSuccess)
    , ("M-d r"              , addName "Recompile XMonad"               $ spawn "xmonad-restart")
    , ("M-d c"              , addName "Restart XMonad"                 $ spawn "xmonad-recompile && xmonad-restart")
  ]

-- TODO
  -- subKeys "Move"
  -- (
  -- [
  --   ("M-S-w",                  addName "" $ withFocused (keysMoveWindow ( 0, -3)))
  -- , ("M-S-s",                  addName "" $ withFocused (keysMoveWindow ( 0,  3)))
  -- , ("M-S-a",                  addName "" $ withFocused (keysMoveWindow (-3,  0)))
  -- , ("M-S-d",                  addName "" $ withFocused (keysMoveWindow ( 3,  0))) 
  -- ]
  -- )

  -- Window Floating Movement Hotkey
  -- , ("M-M1-S-w",                  addName "" $ withFocused (keysResizeWindow ( 0, -1) ( 0,  0)))
  -- , ("M-M1-S-s",                  addName "" $ withFocused (keysResizeWindow ( 0,  1) ( 0,  0)))
  -- , ("M-M1-S-a",                  addName "" $ withFocused (keysResizeWindow (-1, -0) ( 0,  1)))
  -- , ("M-M1-S-d",                  addName "" $ withFocused (keysResizeWindow ( 1,  0) ( 0,  0)))

myEzRemovedKeybindings :: [String]
myEzRemovedKeybindings =
  [
    "M-q"
  ]

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr

doDialogCenterFloat :: Query (Endo WindowSet)
doDialogCenterFloat = doRectFloat (StackSet.RationalRect 0 0 0.6 0.6) >> doCenterFloat

windowRole :: Query String
windowRole = stringProperty "WM_WINDOW_ROLE"

-- | `q =* x`
-- | if the result of `q` contains `x`, return `True`.
(=*) :: Query String   -- | Query for window's property
        -> String      -- | Substring to match
        -> Query Bool
q =* x = fmap (isInfixOf x) q

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
     composeAll
     [ className =? "confirm"         --> doCenterFloat
     , className =? "file_progress"   --> doCenterFloat
     , className =? "dialog"          --> doCenterFloat
     , className =? "Zenity"          --> doCenterFloat
     , className =? "download"        --> doCenterFloat
     , className =? "error"           --> doCenterFloat
     , className =? "notification"    --> doCenterFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doCenterFloat
     , className =? "toolbar"         --> doFloat
     , className =? "feh"             --> doFullFloat
     , className =? "Sxiv"            --> doFullFloat
     , className =? "Veracrypt"       --> doCenterFloat
     , isDialog                       --> doCenterFloat

     -- , className =? "obs" <&&> title =? "Filter name"
        -- --> doCenterFloat

     , windowRole =? "pop-up"
        --> doFloat

     , title =? "The Elder Scrolls Online Install"
        --> doCenterFloat

     , className =? "Steam" <&&> title =* "Steam - News"
        --> doShift (myWorkspaces !! 5)

     , title =? "Task Manager - Chromium"
        --> doDialogCenterFloat

     , className =? "Dragon-drag-and-drop"
        --> doCenterFloat

     , windowRole =? "gimp-message-dialog"
        --> doCenterFloat

     , windowRole =? "GtkFileChooserDialog"
        --> doDialogCenterFloat

     , title =? "Oracle VM VirtualBox Manager"
        --> doCenterFloat

     , title =? "Media viewer" <&&> className =? "TelegramDesktop"
        --> doFullFloat

     ] <+> manageDocks
       <+> manageHook def
     -- <+> namedScratchpadManageHook myScratchPads

     -- TODO: add `doShift`s 
     -- , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
     -- , className =? "brave-browser"   --> doShift ( myWorkspaces !! 1 )
     -- , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
     -- , className =? "mpv"             --> doShift ( myWorkspaces !! 7 )
     -- , className =? "Gimp"            --> doShift ( myWorkspaces !! 8 )
