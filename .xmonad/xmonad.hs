import Data.Map (fromList)
import Data.Maybe (fromJust)
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
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

myBrowser = "firefox"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth = 2

myModMask = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor = "#424242"

myFocusedBorderColor = "#18ffff"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys =
  [ ("M-S-<Return>", spawn myTerminal)
    -- Launch rofi
  , ("M-p", spawn "rofi -show run")
    -- Launch default browser
  , ("M-f", spawn myBrowser)
    -- Launch default browser (firefox in this case) in private mode
  , ("M-S-f", spawn (myBrowser ++ " --private-window"))
    -- Launch chromium in incognito mode
  , ("M-g", spawn "chromium --incognito")
    -- Launch PulseMixer
  , ("M-s", spawn (myTerminal ++ " -e pulsemixer"))
    -- Increase brightness by 10%
  , ("M-<F6>", spawn "xbacklight -inc 10")
    -- Decrease brightness by 10%
  , ("M-<F5>", spawn "xbacklight -dec 10")
  , ("M-<F10>", spawn "toggle-bluetooth")
   --Take a screenshot of entire display
  , ("M-<Print>", spawn "scrot ~/Pictures/Screenshots/screen-%Y-%m-%d-%H-%M-%S.png -d 1")
   --Take a screenshot of focused window
  , ("M-C-<Print>", spawn "scrot ~/Pictures/Screenshots/window-%Y-%m-%d-%H-%M-%S.png -d 1-u")
   --Take a screenshot of chosen area
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
    -- Lock screen
  , ("M-S-l", spawn "slock")
    -- Suspend system
  , ("M-S-s", spawn "systemctl suspend")
    -- Quit xmonad (don't want to accidentally close session)
  , ("M-S-q", return ())
    -- Restart xmonad
  , ("M-q", spawn "xmonad --recompile; xmonad --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
  , ("M-S-/", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
  ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
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

------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
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
    { fontName = "xft:JetBrainsMono:pixelsize=14:antialias=true"
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

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll []

------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xMobar <- spawnPipe "xmobar -x 0"
  xmonad $ docks ((defaultSettings xMobar) `additionalKeysP` myKeys)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
xmobarPrettyPrinting xMobar =
  dynamicLogWithPP
    xmobarPP
      { ppOutput = hPutStrLn xMobar
      , ppCurrent = xmobarColor "#b2ff59" "" . wrap "[" "]" -- Current workspace in xmobar
      , ppVisible = xmobarColor "#18ffff" "" -- Visible but not current workspace
      , ppHidden = xmobarColor "#40c4ff" "" . wrap "*" "" -- Hidden workspaces in xmobar
      , ppHiddenNoWindows = xmobarColor "#ff4081" "" -- Hidden workspaces (no windows)
      , ppTitle = xmobarColor "#64ffda" "" . shorten 50 -- Title of active window in xmobar
      , ppLayout = xmobarColor "#eeff41" ""
      , ppSep = "<fc=#eeeeee> | </fc>" -- Separators in xmobar
      , ppUrgent = xmobarColor "#ff5252" "" . wrap "!" "!" -- Urgent workspace
      , ppExtras = [] -- # of windows current workspace
      }

defaultSettings xMobar =
  def
      -- simple stuff
    { terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , clickJustFocuses = myClickJustFocuses
    , borderWidth = myBorderWidth
    , modMask = myModMask
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
      -- key bindings
    , mouseBindings = myMouseBindings
      -- hooks, layouts
    , layoutHook = myLayout
    , manageHook = manageDocks <+> myManageHook
    , handleEventHook = myEventHook
    , logHook = myLogHook <+> (xmobarPrettyPrinting xMobar)
    , startupHook = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "The default modifier key is 'alt'. Default keybindings:"
    , ""
    , "-- launching and killing programs"
    , "mod-Shift-Enter  Launch xterminal"
    , "mod-p            Launch dmenu"
    , "mod-Shift-p      Launch gmrun"
    , "mod-Shift-c      Close/kill the focused window"
    , "mod-Space        Rotate through the available layout algorithms"
    , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
    , "mod-n            Resize/refresh viewed windows to the correct size"
    , ""
    , "-- move focus up or down the window stack"
    , "mod-Tab        Move focus to the next window"
    , "mod-Shift-Tab  Move focus to the previous window"
    , "mod-j          Move focus to the next window"
    , "mod-k          Move focus to the previous window"
    , "mod-m          Move focus to the master window"
    , ""
    , "-- modifying the window order"
    , "mod-Return   Swap the focused window and the master window"
    , "mod-Shift-j  Swap the focused window with the next window"
    , "mod-Shift-k  Swap the focused window with the previous window"
    , ""
    , "-- resizing the master/slave ratio"
    , "mod-h  Shrink the master area"
    , "mod-l  Expand the master area"
    , ""
    , "-- floating layer support"
    , "mod-t  Push window back into tiling; unfloat and re-tile it"
    , ""
    , "-- increase or decrease number of windows in the master area"
    , "mod-comma  (mod-,)   Increment the number of windows in the master area"
    , "mod-period (mod-.)   Deincrement the number of windows in the master area"
    , ""
    , "-- quit, or restart"
    , "mod-Shift-q  Quit xmonad"
    , "mod-q        Restart xmonad"
    , "mod-[1..9]   Switch to workSpace N"
    , ""
    , "-- Workspaces & screens"
    , "mod-Shift-[1..9]   Move client to workspace N"
    , "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3"
    , "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3"
    , ""
    , "-- Mouse bindings: default actions bound to mouse events"
    , "mod-button1  Set the window to floating mode and move by dragging"
    , "mod-button2  Raise the window to the top of the stack"
    , "mod-button3  Set the window to floating mode and resize by dragging"
    ]
