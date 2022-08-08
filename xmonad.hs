-- IMPORTS
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.ManageHook

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes

import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Theme

import XMonad.Layout.Decoration
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Tabbed

import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.NoBorders


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask            = mod4Mask             -- Rebind Mod to the Super key
    , terminal           = myTerminal           -- Set default terminal
    , layoutHook         = myMadLayout          -- Use custom layouts
    , manageHook         = myManageHook         -- Match on certain windows
    , startupHook        = myStartupHook        -- Define application to run at startup
    , borderWidth        = myBorderWidth        -- BorderWidth
    , normalBorderColor  = myNormalBorderColor  -- Border color for unfocused window
    , focusedBorderColor = myFocusedBorderColor -- Border color for focused window
    , keys               = myKeys <+> keys def  -- Extending key binding
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-e", spawn "emacsclient -nc")
    , ("M-C-s", unGrab *> spawn "scrot -s"        )
    , ("M-p", spawn "my_dmenu.sh")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
            [ ((modm, xK_F12), xmonadPrompt def)
            , ((modm, xK_r ), shellPrompt myShellPrompt)
            , ((modm .|. controlMask, xK_t), themePrompt myThemePrompt)
            , ((modm,  xK_g ),   withFocused toggleBorder)
            ]

myBorderWidth :: Dimension
myBorderWidth = 1

myTerminal :: String
myTerminal = "xfce4-terminal"

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#444444"
myFocusedBorderColor = "#222222"

myLayout = tiled ||| Full
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myTheme = def { activeColor         = "#222222"
              , inactiveColor       = "#2d2d2d"
              , urgentColor         = "#252525"
              , activeBorderColor   = "#222222"
              , inactiveBorderColor = "#2d2d2d"
              , urgentBorderColor   = "#252525"
              , activeTextColor     = "#999999"
              , inactiveTextColor   = "#777777"
              , urgentTextColor     = "#ffffff"
              , fontName            = "xft:Cantarell:size=9"
              , decoWidth           = 400
              , decoHeight          = 20
              }

-- myMadLayout = tallTabbed shrinkText myTheme ||| myLayout
myMadLayout = tabbed shrinkText myTheme ||| myLayout

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "emacs --daemon &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "compton &"

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myShellPrompt :: XPConfig
myShellPrompt = def
  { font              = "xft:JetBrainsMono Nerd Font:pixelsize=13"
  , bgColor           = "#222222"
  , fgColor           = "#aaaaaa"
  , bgHLight          = "#005577"
  , fgHLight          = "#ffffff"
  , borderColor       = "#222222"
  , promptBorderWidth = 1
  , position          = Top
  , maxComplRows      = Just 10
  }

myThemePrompt :: XPConfig
myThemePrompt = def
  { font              = "xft:JetBrainsMono Nerd Font:pixelsize=13"
  , bgColor           = "#222222"
  , fgColor           = "#aaaaaa"
  , bgHLight          = "#005577"
  , fgHLight          = "#ffffff"
  , borderColor       = "#222222"
  , promptBorderWidth = 1
  , position          = Top
  }
