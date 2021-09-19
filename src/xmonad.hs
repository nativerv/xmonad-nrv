{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

-- XMonad
import XMonad
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Promote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Actions.WithAll

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

-- Base
import qualified Data.Map as M
import Data.Monoid
import System.IO
import System.Exit

-- Theme
import Theme.Theme
import XMonad.Hooks.StatusBar.PP (xmobarFont)
import Data.List (isInfixOf)

main :: IO ()
main = do
  xmobarProc <- spawnPipe "killall xmobar; xmobar"

  xmonad
    $ ewmh
    $ withNavigation2DConfig myNav2DConfig
    $ addDescrKeys' ((myModMask .|. shiftMask, xK_slash), showKeybindings) myKeys
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

myTerminal :: String
myTerminal = "st"

myLauncher :: String
myLauncher = "dmenu_run"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 0

myOuterGapWidth, myInnerGapWidth :: Integer
myOuterGapWidth = 15
myInnerGapWidth = 5

centerPointerLogHook :: X ()
centerPointerLogHook = updatePointer (0.5, 0.5) (0, 0)

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
  , ppVisible          = wsIconFont . wsNonemptyColor . wrap "(" ")"          -- | Visible with windows (on another monitor)
  , ppVisibleNoWindows = Just $ wsIconFont . wsNonemptyColor . wrap "(" ")"   -- | Visible with no windows (on another monitor)
  , ppHidden           = wsIconFont . wsNonemptyColor                         -- | Hidden with no windows
  , ppCurrent          = wsIconFont . wsActiveColor                           -- | Visible (on current monitor)
  , ppHiddenNoWindows  = wsIconFont                                           -- | Not on any monitor and having no windows
  , ppUrgent           = wsUrgentColor                                        -- | Urgent (window on this tag wants your focus)
  , ppTitle            = titleColor
  , ppLayout           = layoutAction . layoutColor
  , ppSep              = xmobarColor colorGrey "" " | "
  }
  where
    wsIconFont    = xmobarFont 5
    wsActiveColor = xmobarColor color10 ""
    wsNonemptyColor = xmobarColor color01 ""
    wsUrgentColor = xmobarColor "#e46e49" ""
    titleColor    = xmobarColor color01 ""
    layoutAction  = xmobarAction ("xdotool key " <> modMaskName myModMask <> "+space") "1"
    layoutColor   = xmobarColor color06 ""

fadeInactiveLogHook' :: X ()
fadeInactiveLogHook' = fadeInactiveLogHook 0.9

myLogHook dest =
  centerPointerLogHook
  <+> xmobarLogHook dest
  -- <+> fadeInactiveLogHook'

-- myFocusedBorderColor = "#10EEFF"
myFocusedBorderColor = "#EEEEEE"
-- , borderColor = "#0055FF"

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

modMaskName :: KeyMask -> String
modMaskName modMaskUsed
  | modMaskUsed == mod1Mask = "alt"
  | modMaskUsed == mod2Mask = "Num_Lock"
  | modMaskUsed == mod3Mask = undefined
  | modMaskUsed == mod4Mask = "super"
  | modMaskUsed == mod5Mask = undefined
  | otherwise = undefined


myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $ ["爵", "\61728", "\61802", "\61723", "\57871"]
  where
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
        (index, workspace) <- zip [1..5::Int] workspaceList
      ]

myInnerGapBorder :: Border
myInnerGapBorder = Border myInnerGapWidth myInnerGapWidth myInnerGapWidth myInnerGapWidth

myLayoutHook = tall ||| full
  where
    named n = renamed [XMonad.Layout.Renamed.Replace n]
    --                         MasterCount ResizeAmount MasterSize SomeShit
    tall = named "[]="
      $ smartBorders
      $ avoidStruts
      $ spacingRaw False myInnerGapBorder True myInnerGapBorder True
      $ layoutHints
      $ ResizableTall 1           (1/30)       0.5        []

    full = named "[ ]"
      $ smartBorders
      $ avoidStruts
      $ spacingRaw False myInnerGapBorder True myInnerGapBorder True
      $ layoutHints
      $ Full

myHandleEventHook :: Event -> X All
myHandleEventHook = hintsEventHook <+> docksEventHook <+> handleEventHook def

wsKeys :: [String]
wsKeys = map show $ [1..9::Int] ++ [0]

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf =
  let

  subKeys str ks = subtitle str : mkNamedKeymap conf ks
  screenKeys     = ["w","v","z"]
  dirKeys        = ["j","k","h","l"]
  dirs           = [ D,  U,  L,  R ]

  -- Zip with modKey
  zipM  modKey name actionKeys actions f = zipWith (\key d -> (modKey ++ key, name $ f d)) actionKeys actions
  zipM' modKey name actionKeys actions f b = zipWith (\key d -> (modKey ++ key, name $ f d b)) actionKeys actions

  in

  subKeys "Window"
  (
  [
      ("M-<Backspace>"      , addName "Kill This Copy Of Focused"          kill1)
    , ("M-S-<Backspace>"    , addName "Kill All Copies Of Focused"         kill)
    , ("M-M1-<Backspace>"   , addName "Kill All On Current Workspace"      kill)
    , ("M-b"                , addName "Promote To Master"                  promote)
    , ("M-S-,"                , addName "Increment Master Size"              $ sendMessage (IncMasterN 1))
    , ("M-S-."                , addName "Decrement Master Size"              $ sendMessage (IncMasterN (-1)))
    , ("M-<Tab>"            , addName "Cycle Focus"                        $ windows W.focusDown)
    , ("M-S-<Tab>"          , addName "Cycle Focus Back"                   $ windows W.focusUp)
    , ("M-z m"              , addName "Focus Master"                       $ windows W.focusMaster)
    , ("M-t"                , addName "Tile Window"                        $ withFocused $ windows . W.sink)
    , ("M-f"                , addName "FullFloat Window"                   $ withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1))
    , ("M-n"                , addName "Refresh (fix client sizes)"         $ refresh)
  ]
    ++ zipM' "M-"            (addName "Navigate")                          dirKeys dirs windowGo False
    ++ zipM' "M-C-"          (addName "Swap")                              dirKeys dirs windowSwap False
    ++ zipM  "M-S-"          (addName "Move To Workspace")                 wsKeys [0..] (withNthWorkspace W.shift)
    ++ zipM  "M-C-"          (addName "Copy To Workspace")                 wsKeys [0..] (withNthWorkspace copy)
  )
  ^++^

  subKeys "Workspace"
  (
  [
      ("M-i"                , addName "Cycle Left"                         $ prevWS) -- moveTo Prev AnyWS
    , ("M-o"                , addName "Cycle Right"                        $ nextWS) -- moveTo Next AnyWS
  ]
    ++ zipM "M-"             (addName "Switch to workspace")               wsKeys [0..] (withNthWorkspace W.greedyView)
 )
  ^++^

  subKeys "Layout"
  [
      ("M-<Space>"          , addName "Cycle Layout"                       $ sendMessage NextLayout)
    , ("M-S-<Space>"        , addName "Set Layout To Default"              $ setLayout (XMonad.layoutHook conf))
  ]
  ^++^

  subKeys "Spawn"
  [
      ("M-S-<Return>"       , addName "Terminal"                           $ spawn myTerminal)
    , ("M-p"                , addName "Launcher"                           $ spawn myLauncher)
  ]
   ^++^

  subKeys "Resize"
  [
      ("M-S-h"              , addName "Resize Left"                        $ sendMessage Shrink)
    , ("M-S-l"              , addName "Resize Right"                       $ sendMessage Expand)
    , ("M-S-j"              , addName "Resize Down"                        $ sendMessage MirrorShrink)
    , ("M-S-k"              , addName "Resize Up"                          $ sendMessage MirrorExpand)
  ]
  ^++^

  subKeys "Spacing"
  [
      ("M-["                , addName "Decrease Spacing"                   $ incScreenWindowSpacing 1)
    , ("M-]"                , addName "Increase Spacing"                   $ incScreenWindowSpacing (-1))
    , ("M-<Home>"           , addName "Reset Spacing"                      $ setScreenWindowSpacing myInnerGapWidth)

  ] ^++^

  subKeys "System"
  [
      ("M-S-q"              , addName "Quit XMonad"                        $ io exitSuccess)
    , ("M-d r"              , addName "Recompile XMonad"                   $ spawn "xmonad-restart")
    , ("M-d c"              , addName "Restart XMonad"                     $ spawn "xmonad-recompile && xmonad-restart")
  ]

-- TODO
  -- subKeys "Move"
  -- (
  -- [
  -- ("M-S-w",                  addName "" $ withFocused (keysMoveWindow ( 0, -3)))
  -- , ("M-S-s",                  addName "" $ withFocused (keysMoveWindow ( 0,  3)))
  -- , ("M-S-a",                  addName "" $ withFocused (keysMoveWindow (-3,  0)))
  -- , ("M-S-d",                  addName "" $ withFocused (keysMoveWindow ( 3,  0))) 
  -- ]
  -- )

    -- -- Window Floating Movement Hotkey
    -- , ("M-M1-S-w",                  addName "" $ withFocused (keysResizeWindow ( 0, -1) ( 0,  0)))
    -- , ("M-M1-S-s",                  addName "" $ withFocused (keysResizeWindow ( 0,  1) ( 0,  0)))
    -- , ("M-M1-S-a",                  addName "" $ withFocused (keysResizeWindow (-1, -0) ( 0,  1)))
    -- , ("M-M1-S-d",                  addName "" $ withFocused (keysResizeWindow ( 1,  0) ( 0,  0)))

myEzRemovedKeybindings :: [String]
myEzRemovedKeybindings =
  [
      "M-q"
  ]
  -- , "M-f"

myStartupHook :: X ()
myStartupHook = do
    setDefaultCursor xC_left_ptr


doDialogCenterFloat :: Query (Endo WindowSet)
doDialogCenterFloat = doRectFloat (W.RationalRect 0 0 0.6 0.6) >> doCenterFloat

windowRole :: Query String
windowRole = stringProperty "WM_WINDOW_ROLE"

-- | @q =* x@. if the result of @q@ contains @x@, return 'True'.
(=*) :: Query String -> String -> Query Bool
q =* x = fmap (isInfixOf x) q

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
     composeAll
     [ className =? "confirm"            --> doCenterFloat
     , className =? "file_progress"      --> doCenterFloat
     , className =? "dialog"             --> doCenterFloat
     , className =? "Zenity"             --> doCenterFloat
     , className =? "download"           --> doCenterFloat
     , className =? "error"              --> doCenterFloat
     , className =? "notification"       --> doCenterFloat
     , className =? "pinentry-gtk-2"     --> doFloat
     , className =? "splash"             --> doCenterFloat
     , className =? "toolbar"            --> doFloat
     , className =? "feh"                --> doFullFloat
     , className =? "Sxiv"               --> doFullFloat
     , className =? "Veracrypt"          --> doCenterFloat
     , isDialog                          --> doCenterFloat

     -- , className =? "obs" <&&> title =? "Filter name"
        -- --> doCenterFloat

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
