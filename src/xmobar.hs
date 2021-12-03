import Xmobar
import XMonad (spawn, xmessage)
import qualified Theme.Theme as T

import System.Environment
import System.FilePath
import System.IO.Unsafe
import XMonad.Hooks.DynamicLog (xmobarAction)
import Util.Util ((<|>))
import Common.Common ()
import System.IO
import System.Process
import XMonad.Hooks.StatusBar.PP (wrap, xmobarFont)
import XMonad.Hooks.StatusBar.PP (xmobarColor)

-- Example user-defined plugin
data HelloWorld = HelloWorld
    deriving (Read, Show)

instance Exec HelloWorld where
    alias HelloWorld = "hw"
    run   HelloWorld = return "<fc=red>Hello World!!</fc>"

homeDir :: String
homeDir = unsafeDupablePerformIO (getEnv "HOME")

localBinDir :: String
localBinDir = homeDir </> ".local" </> "bin"

localBinary :: String -> String
localBinary binaryName = localBinDir </> binaryName

networkDeviceName :: String
networkDeviceName = init $ unsafeDupablePerformIO $ readProcess "xmobar-get-current-network-device" [] []

myConfig :: Config
myConfig = defaultConfig {
    font            =   "xft:monospace:weight=bold:pixelsize=10:antialias=true:hinting=true"
  , additionalFonts = [ "xft:FiraCode Nerd Font Mono:pixelsize=11:antialias=true:hinting=true"
                      , "xft:Font Awesome 5 Free Solid:pixelsize=12"
                      , "xft:Font Awesome 5 Brands:pixelsize=12"
                      , "xft:monospace:pixelsize=26"
                      , "xft:monospace:pixelsize=20"
                      , "xft:monospace:pixelsize=10"   -- % Spaces around workspace icons
                      ]
  , bgColor = T.bgColor
  , fgColor = T.fgColor
  -- , position = Static { xpos = 0 , ypos = 0, width = 1920, height = 18 }
  , position = TopSize L 100 T.myBarHeight
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , iconRoot = homeDir </> ".xmonad" </> "xpm"
  , commands = [
     -- Time and date
      Run $ Date "%a %b %d %Y - %H:%M " "date" 50

      -- Network up and down
    --, Run $ Network (networkDeviceName <> "_down") ["-c", " ", "-m", "4", "-a", "l", "-t", "<rx>"] 20
    --, Run $ Network (networkDeviceName <> "_up") ["-c", " ", "-m", "4", "-a", "l", "-t", "<tx>"] 20
    , Run $ Com (localBinary "sb-nettraffic") ["rx"] "rx" 20
    , Run $ Com (localBinary "sb-nettraffic") ["tx"] "tx" 20

      -- Cpu usage in percent
    , Run $ Cpu ["-t", "<total>%", "-m", "3", "-c", " ", "-p", "3", "-H", "50", "--high", "red"] 20

      -- Ram used number and percent
    --, Run $ Memory ["-c", " ", "-m", "4", "-t", "<used>"] 20
    , Run $ Com (localBinary "sb-memory") ["used"] "mem" 20

      -- Disk space free
    , Run $ Com (localBinary "sb-disk") ["/"] "disk" 60

      -- Runs custom script to check for pacman updates.
      -- This script is in my dotfiles repo in .local/bin.
    , Run $ Com (localBinary "pacman-get-updatable-count") [] "pacman_updatable_count" 300

      -- Runs a standard shell command 'uname -r' to get kernel version
    -- , Run $ Com "uname" ["-r"] "" 3600

      -- Runs a shell script outputting empty XPM icon of trayer width
      -- to offset the rest of the bar from it
    , Run $ Com (localBinary "xmobar-trayer-padding-icon-gen") [] "trayer_spacing" 10

    -- Read stdin, to which xmonad pipes tags and stuff
    , Run UnsafeStdinReader
  ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = myTemplate
}

separator :: String
separator = xmobarColor "#666666" "" "|"

myTemplate :: String
myTemplate = unwords
  [ ""
  , xmobarAction "dmenu_run" "1" $ xmobarFont 4 "\58911"
  , separator
  , "%UnsafeStdinReader%"
  , "}{"
  , xmobarAction "st -e htop" "1" $ coloredIcon "\62171" <|> "%cpu%"
  , separator
  , xmobarAction "st -e htop" "1" $ coloredIcon "\xf233" <|> "%mem%"
  , separator
  , xmobarAction "st -e ncdu" "1" $ coloredIcon "\xf0c7" <|> "%disk%"
  , separator
  , xmobarAction "sudo ifto -i" "1" $ coloredIcon "\xf0ab" <|> "%rx%"
  , xmobarAction "sudo iftop -i" "1" $ coloredIcon "\xf0aa" <|> "%tx%"
  , separator
  , xmobarAction "st -e pacman-print-updatable-and-update" "1" $ coloredIcon "\61683" <|> "%pacman_updatable_count%"
  , separator
  , xmobarAction "echo" "1" $ coloredIcon "\xf017" <|> "%date%"
  , separator
  ] <> "%trayer_spacing%"
  where
    coloredIcon = xmobarColor T.color03 "" . xmobarFont 2

main :: IO ()
main = spawn trayerCmd >> putStrLn (wrap "\"" "\"" myTemplate) >> xmobar myConfig
  where
    trayerCmd = "killall trayer; trayer " <> unwords trayerArgs
    trayerArgs =
      [ "--edge top"
      , "--align right"
      , "--widthtype request"
      , "--padding 6"
      , "--SetDockType true"
      , "--SetPartialStrut true"
      , "--expand true"
      , "--monitor 0"
      , "--transparent true"
      , "--alpha 0"
      , "--tint " <> hexBgColor
      , "--height " <> show T.myBarHeight
      ]
    hexBgColor = "0x" <> tail T.bgColor

legacyTemplate :: String
legacyTemplate = " <action=`dmenu_run`><fn=4>\58911</fn></action> <fc=#666666>|</fc> %UnsafeStdinReader% }{ <fc=#666666></fc><fc=#ecbe7b> <action=`st -e htop`><fn=2>\62171</fn> %cpu%</action> </fc><fc=#666666>|</fc><fc=#ff6c6b> <action=`st -e htop`>%memory%</action> </fc><fc=#666666>|</fc><fc=#51afef> <action=`st -e htop`>%disku%</action> </fc><fc=#666666>|</fc><fc=#98be65> <action=`st -e sudo iftop -i wlp0s26u1u3`>%wlp0s26u1u3%</action> </fc><fc=#666666>|</fc> <fc=#c678dd><fn=2>\61683</fn> <action=`st -e pacman-print-updatable-and-update`>%pacman_updatable_count%</action> </fc><fc=#666666>|</fc><fc=#46d9ff> <action=`emacsclient -c -a 'emacs' --eval '(doom/window-maximize-buffer(dt/year-calendar))'`>%date%</action> </fc><fc=#666666>|</fc> %trayer_spacing%"
