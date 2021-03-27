import Data.Map (fromList)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Tree
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.TreeSelect
import XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myTerminal = "alacritty"

myFont = "xft:JetBrainsMono Nerd Font:pixelsize=14:antialias=true:hinting=true"

-- Key bindings
--
myKeys =
  [ ("M-S-<Return>", spawn myTerminal)
    -- Tree select actions
  , ("M-a", actions treeSelectConfig)
    -- Launch file manager
  , ("M-e", spawn (myTerminal ++ " -e ranger"))
    -- Launch rofi
  , ("M-p", spawn "rofi -show run")
    -- Launch firefox
  , ("M-f", spawn "firefox")
    -- Launch firefox in private mode
  , ("M-S-f", spawn "firefox --private-window")
    -- Launch chromium
  , ("M-g", spawn "chromium")
    -- Launch chromium in incognito mode
  , ("M-S-g", spawn "chromium --incognito")
    -- Launch PulseMixer
  , ("M-s", spawn (myTerminal ++ " -e pulsemixer"))
    -- Take a screenshot of entire display
  , ("M-<Print>", spawn "scrot ~/Pictures/Screenshots/screen-%Y-%m-%d-%H-%M-%S.png -d 1")
    -- Take a screenshot of focused window
  , ("M-C-<Print>", spawn "scrot ~/Pictures/Screenshots/window-%Y-%m-%d-%H-%M-%S.png -d 1-u")
    -- Take a screenshot of chosen area
  , ("M-S-<Print>", spawn "scrot ~/Pictures/Screenshots/area-%Y-%m-%d-%H-%M-%S.png -s 1-u")
    -- Close focused window
  , ("M-S-c", kill)
    -- Rotate through the available layout algorithms
  , ("M-<Space>", sendMessage NextLayout)
    -- Resize viewed windows to the correct size
  , ("M-n", refresh)
    -- Move focus to the next window
  , ("M-<Tab>", windows W.focusDown)
    -- Move focus to the next window
  , ("M-j", windows W.focusDown)
    -- Move focus to the previous window
  , ("M-k", windows W.focusUp)
    -- Move focus to the master window
  , ("M-m", windows W.focusMaster)
    -- Swap the focused window and the master window
  , ("M-<Return>", windows W.swapMaster)
    -- Swap the focused window with the next window
  , ("M-S-j", windows W.swapDown)
    -- Swap the focused window with the previous window
  , ("M-S-k", windows W.swapUp)
    -- Shrink the master area
  , ("M-h", sendMessage Shrink)
    -- Expand the master area
  , ("M-l", sendMessage Expand)
    -- Push window back into tiling
  , ("M-t", withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))
    -- Decrement the number of windows in the master area
  , ("M-.", sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
  , ("M-b", sendMessage ToggleStruts)
    -- Quit xmonad (don't want to accidentally kill session)
  , ("M-S-q", return ())
    -- Restart xmonad
  , ("M-q", spawn "xmonad --recompile; xmonad --restart")
  ]

-- Mouse bindings
--
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-- Layouts
--
defaultTallLayout = Tall nMaster delta ratio
  where
    nMaster = 1
    delta = 2 / 100
    ratio = 1 / 2

tallLayout = renamed [Replace "Tall"] (defaultSpacing defaultTallLayout)

-- mirrorLayout = defaultSpacing (Mirror defaultTallLayout)
-- gridLayout = renamed [Replace "Grid"] (defaultSpacing Grid)
-- fullLayout = noBorders Full
--
tabbedLayout = renamed [Replace "Tabbed"] (noBorders (tabbedBottomAlways shrinkText myTabbedTheme))

myTabbedTheme =
  def
    { fontName = myFont
    , activeColor = "#b2ff59"
    , inactiveColor = "#424242"
    , activeBorderColor = "#b2ff59"
    , inactiveBorderColor = "#424242"
    , activeTextColor = "#333333"
    , inactiveTextColor = "#ffffff"
    }

myLayout = avoidStruts (tabbedLayout ||| tallLayout)

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

defaultSpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
defaultSpacing = mySpacing 4

-- Tree select
--
treeSelectConfig =
  TSConfig
    { ts_hidechildren = True
    , ts_background = 0xea333333
    , ts_font = myFont
    , ts_node = (0xff000000, 0xff00bfa5)
    , ts_nodealt = (0xff000000, 0xff1de9b6)
    , ts_highlight = (0xffffffff, 0xff00695c)
    , ts_extra = 0xff000000
    , ts_node_width = 200
    , ts_node_height = 30
    , ts_originX = 0
    , ts_originY = 0
    , ts_indent = 80
    , ts_navigate = defaultNavigation
    }

empty = ""

parentAction name children = Node (TSNode name empty (return ())) children

childAction name action = Node (TSNode name empty action) []

actions config =
  treeselectAction
    config
    [ parentAction
        "System"
        [ childAction "Lock" (spawn "slock")
        , childAction "Suspend" (spawn "systemctl suspend")
        , childAction "Kill session" (io (exitWith ExitSuccess))
        , childAction "Shutdown" (spawn "shutdown now")
        , childAction "Reboot" (spawn "reboot")
        ]
    , parentAction
        "Brightness"
        [ childAction "100%" (spawn "xbacklight -set 100")
        , childAction "80%" (spawn "xbacklight -set 80")
        , childAction "60%" (spawn "xbacklight -set 60")
        , childAction "40%" (spawn "xbacklight -set 40")
        , childAction "20%" (spawn "xbacklight -set 20")
        , childAction "0%" (spawn "xbacklight -set 0")
        ]
    , childAction "Bluetooth" (spawn "toggle-bluetooth")
    ]

-- Window rules
--
myManageHook = composeAll []

-- Event handling
--
myEventHook = mempty

-- Status bars and logging
--
myLogHook = return ()

-- Startup hook
--
keyboardLayout = "setxkbmap -layout us,pl,ru,ua -option grp:alt_shift_toggle"

swapCapsEsc = "setxkbmap -option caps:swapescape"

typingRepeatSpeed = "xset r rate 200 35"

cursor = "xsetroot -cursor_name left_ptr"

wallpapers = "nitrogen --restore &"

compositor = "picom --config ~/.config/picom/picom.conf &"

myStartupHook = do
  spawn keyboardLayout
  spawn swapCapsEsc
  spawn typingRepeatSpeed
  spawn cursor
  spawn wallpapers
  spawn compositor

-- Main
--
main = do
  xMobar <- spawnPipe "xmobar -x 0"
  xmonad $ docks ((defaultSettings xMobar) `additionalKeysP` myKeys)

xmobarPrettyPrinting xMobar =
  dynamicLogWithPP
    xmobarPP
      { ppOutput = hPutStrLn xMobar
        -- Current workspace in xmobar
      , ppCurrent = xmobarColor "#b2ff59" "" . wrap "[" "]"
        -- Visible but not current workspace
      , ppVisible = xmobarColor "#18ffff" ""
        -- Hidden workspaces in xmobar
      , ppHidden = xmobarColor "#40c4ff" "" . wrap "*" ""
        -- Hidden workspaces (no windows)
      , ppHiddenNoWindows = xmobarColor "#ff4081" ""
        -- Title of active window in xmobar
      , ppTitle = xmobarColor "#64ffda" "" . shorten 50
      , ppLayout = xmobarColor "#eeff41" ""
        -- Separators in xmobar
      , ppSep = "<fc=#eeeeee> | </fc>"
        -- Urgent workspace
      , ppUrgent = xmobarColor "#ff5252" "" . wrap "!" "!"
        -- # of windows current workspace
      , ppExtras = []
      }

defaultSettings xMobar =
  def
    { terminal = myTerminal
    , focusFollowsMouse = True
    , clickJustFocuses = False
    , borderWidth = 2
    , modMask = mod4Mask
    , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , normalBorderColor = "#424242"
    , focusedBorderColor = "#18ffff"
    , mouseBindings = myMouseBindings
    , layoutHook = myLayout
    , manageHook = manageDocks <+> myManageHook
    , handleEventHook = myEventHook
    , logHook = myLogHook <+> (xmobarPrettyPrinting xMobar)
    , startupHook = myStartupHook
    }
