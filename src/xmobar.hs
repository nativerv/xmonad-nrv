import Xmobar
import XMonad (spawn)
import qualified Theme.Theme as T

import System.Environment
import System.FilePath
import System.IO.Unsafe

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

myConfig :: Config
myConfig = defaultConfig {
    font            =   "xft:monospace:weight=bold:pixelsize=10:antialias=true:hinting=true"

  , additionalFonts = [ "xft:FiraCode Nerd Font Mono:pixelsize=11:antialias=true:hinting=true" 
                      , "xft:Font Awesome 5 Free Solid:pixelsize=12"
                      , "xft:Font Awesome 5 Brands:pixelsize=12"
                      , "xft:monospace:pixelsize=26"
                      , "xft:monospace:pixelsize=20"
                      ]
  , bgColor = T.bgColor
  , fgColor = T.fgColor
  -- , position = Static { xpos = 0 , ypos = 0, width = 1920, height = 18 }
  , position = TopSize L 100 T.myBarHeight
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , iconRoot = homeDir </> ".xmonad/xpm"
  , commands = [
     -- Time and date
      Run $ Date "<fn=2>\xf017</fn> %b %d %Y - %H:%M " "date" 50

      -- Network up and down
    , Run $ Network "wlp0s26u1u3" ["-c", " ", "-m", "4", "-a", "l", "-t", "<fn=2>\xf0ab</fn> <rx>KB <fn=2>\xf0aa</fn> <tx>KB"] 20
    
      -- Cpu usage in percent
      -- <fn=2>\xf108</fn> 
    , Run $ Cpu ["-t", "<total>%", "-m", "3", "-c", " ", "-p", "3", "-H", "50", "--high", "red"] 20

      -- Ram used number and percent
    , Run $ Memory ["-c", " ", "-m", "4", "-t", "<fn=2>\xf233</fn> <used>M"] 20

      -- Disk space free
    , Run $ DiskU [("/", "<fn=2>\xf0c7</fn> <free>")] [] 60

      -- Runs custom script to check for pacman updates.
      -- This script is in my dotfiles repo in .local/bin.
    , Run $ Com (localBinary "pacman-get-updatable-count") [] "PacmanUpdateCount" 300

      -- Runs a standard shell command 'uname -r' to get kernel version
    -- , Run $ Com "uname" ["-r"] "" 3600

      -- Runs a shell script outputting empty XPM icon of trayer width
      -- to offset the rest of the bar from it
    , Run $ Com (localBinary "xmobar-trayer-padding-icon-gen") [] "trayerspacing" 10

    -- Read stdin, to which xmonad pipes tags and stuff
    , Run UnsafeStdinReader
  ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " <action=`dmenu_run`><fn=4>\58911</fn></action> <fc=#666666>|</fc> %UnsafeStdinReader% }{ <fc=#666666></fc><fc=#ecbe7b> <action=`st -e htop`><fn=2>\62171</fn> %cpu%</action> </fc><fc=#666666>|</fc><fc=#ff6c6b> <action=`st -e htop`>%memory%</action> </fc><fc=#666666>|</fc><fc=#51afef> <action=`st -e htop`>%disku%</action> </fc><fc=#666666>|</fc><fc=#98be65> <action=`st -e sudo iftop -i wlp0s26u1u3`>%wlp0s26u1u3%</action> </fc><fc=#666666>|</fc> <fc=#c678dd><fn=2>\61683</fn> <action=`st -e pacman-print-updatable-and-update`>%PacmanUpdateCount%</action> </fc><fc=#666666>|</fc><fc=#46d9ff> <action=`emacsclient -c -a 'emacs' --eval '(doom/window-maximize-buffer(dt/year-calendar))'`>%date%</action> </fc><fc=#666666>|</fc> %trayerspacing%"
}

main :: IO ()
main = spawn trayerCmd >> xmobar myConfig
  where
    trayerCmd = "killall trayer; trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 20 --tint " <> hexColor <> " --height " <> show T.myBarHeight
    hexColor = "0x" ++ tail T.bgColor
