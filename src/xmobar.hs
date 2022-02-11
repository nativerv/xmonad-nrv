import Xmobar
import qualified Theme.Theme as T

import System.Environment
import System.FilePath
import System.IO.Unsafe
import XMonad.Hooks.DynamicLog (xmobarAction)
import Util.Util ((<|>))
import Common.Common ()
import XMonad.Hooks.StatusBar.PP ( wrap, xmobarColor, xmobarFont )
import XMonad (spawn)
import System.Process (getProcessExitCode, runInteractiveProcess)
import System.IO (hClose, hSetBinaryMode, BufferMode (LineBuffering), hSetBuffering, hGetLine, hGetContents)

-- | Like Com, but outputs each new line of stdout of provided command
data Listen
  = Listen
      String   
      -- ^ Path
      [String] 
      -- ^ Args
      String   
       -- ^ Alias
    deriving (Read, Show)

instance Exec Listen where
    alias (Listen _ _ a) = a
    start (Listen path args _) cb = do
      (hstdin, hstdout, hstderr, _) <- runInteractiveProcess path args Nothing Nothing
      hClose hstdin
      hClose hstderr
      hSetBinaryMode hstdout False
      hSetBuffering hstdout LineBuffering

      -- Get stdout of spawned process and map it's lines to callback actions
      hGetContents hstdout >>= (mapM_ cb . lines)

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
                      , "xft:Font Awesome 6 Free Solid:pixelsize=12"
                      , "xft:Font Awesome 6 Brands:pixelsize=12"
                      , "xft:monospace:pixelsize=26"
                      , "xft:monospace:pixelsize=20"
                      , "xft:monospace:pixelsize=10"   -- % Spaces around workspace icons
                      , "xft:nrv\\-icons:pixelsize=12"   -- % My custom font with custom icons
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
    --  | Time and date
      Run $ Com (localBinary "sb-date") [] "date" 50

    --  | Network up and down
    , Run $ Com (localBinary "sb-nettraf") ["rx"] "rx" 20
    , Run $ Com (localBinary "sb-nettraf") ["tx"] "tx" 20

    --  | Cpu usage in percent
    , Run $ Cpu ["-t", "<total>%", "-m", "3", "-c", " ", "-p", "3", "-H", "50", "--high", "red"] 20

    --  | Ram used number and percent
    , Run $ Com (localBinary "sb-memory") ["used"] "mem" 20

    --  | Disk space free
    , Run $ Com (localBinary "sb-disk") ["/"] "disk" (10 * 60 * 5)

    --  | Runs custom script to check for pacman updates.
    --  | This script is in my dotfiles repo in .local/bin.
    , Run $ Com (localBinary "pacman-get-updatable-count") [] "pacman_updatable_count" 300

    --  | Runs a standard shell command 'uname -r' to get kernel version
    -- , Run $ Com "uname" ["-r"] "" 3600

    --  | Runs a shell script outputting current MPD song
    , Run $ Listen (localBinary "sb-mus-listen") [] "music"

    --  | Runs a shell script outputting current input language
    , Run $ Listen (localBinary "sb-lang-listen") [] "lang"

    --  | Runs a shell script outputting current volume icon
    , Run $ Listen (localBinary "sb-sink-vol-listen-icon") [] "volume_icon"

    --  | Runs a shell script outputting current microphone icon
    , Run $ Listen (localBinary "sb-source-vol-listen-icon") [] "microphone_icon"

    --  | Runs a shell script outputting empty XPM icon of trayer width
    --  | to offset the rest of the bar from it
    , Run $ Com (localBinary "xmobar-trayer-padding-icon-gen") [] "trayer_spacing" 10

    --  | Read stdin, to which xmonad pipes tags and stuff
    , Run UnsafeStdinReader
  ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = myTemplate
}

myTemplate :: String
myTemplate = unwords
  [ ""
  , xmobarAction "dmenu_run" "1" $ xmobarFont 4 "\58911"
  , separator
  , "%UnsafeStdinReader%"
  , "}"
  , xmobarAction "st -e ncmpcpp" "1" $ coloredIcon "\xf001" <|> "%music%"
  , "{"
  , xmobarAction "st -e htop" "1" $ coloredIcon "\62171" <|> "%cpu%"
  , separator
  , xmobarAction "st -e htop" "1" $ coloredIcon "\xf233" <|> "%mem%"
  , separator
  , xmobarAction "st -e ncdu" "1" $ coloredIcon "\xf0c7" <|> "%disk%"
  , separator
  , xmobarAction "st -e sudo iftop -i" "1" $ coloredIcon "\xf0ab" <|> "%rx%"
  , xmobarAction "st -e sudo iftop -i" "1" $ coloredIcon "\xf0aa" <|> "%tx%"
  , separator
  , xmobarAction "st -e pacman-print-updatable-and-update" "1" $ coloredIcon "\61683" <|> "%pacman_updatable_count%"
  , separator
  , xmobarAction "noop" "1" $ coloredIcon "\xf017" <|> "%date%"
  , separator
  , xmobarAction "noop" "1" "%lang%"
  , xmobarAction "noop" "1" $ xmobarFont 0 "%microphone_icon%"
  , xmobarAction "noop" "1" $ customIconFont "%volume_icon%"
  , "%trayer_spacing%" 
  ]
  where
    iconColor = xmobarColor T.color03 ""
    customIconFont = xmobarFont 7
    spacingFont = xmobarFont 6
    iconFont = xmobarFont 2
    coloredIcon = iconColor . iconFont
    separator = xmobarColor "#666666" "" ""

main :: IO ()
main = spawn trayerCmd >> xmobar myConfig
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
