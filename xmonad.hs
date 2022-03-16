import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops


import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.ClickableWorkspaces

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed

import XMonad.Actions.RotSlaves
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS

import qualified XMonad.StackSet as W


myWorkspaces = ["TERM", "WEBS", "FILE", "DEVS", "DOWN", "MISC"]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-fill ~/Pictures/wallpapers/archcraft-wallpapers/archcraft-backgrounds/files/nord.jpg"

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

tall     = renamed [Replace "Tall"]
           $ spacing 10
           $ Tall 1 (3/100) (1/2)

myLayout = tall ||| Full
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiledb   = spacing 10 $ Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarColor "#8be9fd" "" . wrap (lowWhite "[") (lowWhite "]") -- . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " " "
    , ppHiddenNoWindows = lowWhite . wrap " " " "
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
    white    = xmobarColor "#8be9fd" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#ffffff" ""

myConfig = def
    { modMask            = mod4Mask      -- Rebind Mod to the Super key
    , terminal           = "alacritty"
    , focusFollowsMouse  = False
    , workspaces         = myWorkspaces
    , layoutHook         = myLayout      -- Use custom layouts
    , manageHook         = insertPosition End Newer <+> myManageHook  -- Match on certain windows
    , startupHook        = myStartupHook
    , normalBorderColor  = "#2e3440"
    , focusedBorderColor = "#000"
    , borderWidth        = 2
    }
  `additionalKeysP`
    [ ("M-p"             ,    spawn    "dmenu_run -fn \"Iosevka-10\""  )
    , ("M-k"             ,    windows W.focusDown                      )
    , ("M-j"             ,    windows W.focusUp                        )
    , ("M-S-k"           ,    rotAllDown                               )
    , ("M-S-j"           ,    rotAllUp                                 )
    , ("M-<Right>"       ,    nextWS                                   )
    , ("M-<Left>"        ,    prevWS                                   )
    , ("M-<KP_Add>"      ,    spawn  "/home/luis/.bin/vol-up"          )
    , ("M-<KP_Subtract>" ,    spawn  "/home/luis/.bin/vol-down"        )
    , ("M-<KP_Multiply>" ,    spawn  "/home/luis/.bin/vol-toggle"   )
    ]


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobarrc" (clickablePP myXmobarPP)) defToggleStrutsKey
     $ myConfig

