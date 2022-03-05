import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing

main :: IO ()
main = xmonad def
	{ terminal = "st"
	, borderWidth = 0
    , manageHook=manageHook def <+> manageDocks
    --, layoutHook=spacingWithEdge 10 $ myLayout
    , layoutHook=spacingRaw True (Border 10 0 0 0) True (Border 10 0 0 0) True $ myLayout
	}

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
