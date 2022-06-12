import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.EwmhDesktops

main :: IO ()
main = xmonad
     . ewmh $ myConfig

myConfig = def
     { modMask            = mod4Mask
     , terminal           = "konsole"
     , layoutHook         = myLayout
     , manageHook         = myManageHook
     , borderWidth        = myBorderWidth
     , normalBorderColor  = myNormalBorder
     , focusedBorderColor = myFocusedBorder
     }
     `additionalKeysP`
     [ ("M-C-s", unGrab *> spawn "scrot -s")
     ]
     
     
myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
   where
     threeCol    = ThreeColMid nmaster delta ratio
     tiled       = Tall nmaster delta ratio
     nmaster     = 1
     delta       = 3/100
     ratio       = 1/2
     
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"              --> doFloat
    , className =? "jetbrains-idea-ce" --> doFloat
		, className =? "brave-browser"     --> doFloat
		, className =? "gcolor3"           --> doFloat
    , isDialog                         --> doFloat
    ]
     
myBorderWidth   = 0
myNormalBorder  = "#9c9e9f"
myFocusedBorder = "#97c1f1"
