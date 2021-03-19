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
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  fromList $
    -- Launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- Launch rofi
  , ((modm, xK_p), spawn "rofi -show run")
    -- Launch browser
  , ((modm, xK_g), spawn myBrowser)
    -- Launch browser in private mode
  , ((modm .|. shiftMask, xK_g), spawn (myBrowser ++ " --private-window"))
    -- Launch PulseMixer
  , ((modm, xK_s), spawn (myTerminal ++ " -e pulsemixer"))
    -- Increase brightness by 10%
  , ((modm, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    -- Decrease brightness by 10%
  , ((modm, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
   --Take a screenshot of entire display
  , ((modm, xK_Print), spawn "scrot ~/Pictures/Screenshots/screen-%Y-%m-%d-%H-%M-%S.png -d 1")
   --Take a screenshot of focused window
  , ( (modm .|. controlMask, xK_Print)
    , spawn "scrot ~/Pictures/Screenshots/window-%Y-%m-%d-%H-%M-%S.png -d 1-u")
   --Take a screenshot of chosen area
  , ( (modm .|. shiftMask, xK_Print)
    , spawn "scrot ~/Pictures/Screenshots/area-%Y-%m-%d-%H-%M-%S.png -s 1-u")
    -- Close focused window
  , ((modm .|. shiftMask, xK_c), kill)
     -- Rotate through the available layout algorithms
  , ((modm, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
  , ((modm, xK_n), refresh)
    -- Move focus to the next window
  , ((modm, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
  , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
  , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Decrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
  , ((modm, xK_b), sendMessage ToggleStruts)
    -- Lock screen
  , ((modm .|. shiftMask, xK_l), spawn "slock")
    -- Suspend system
  , ((modm .|. shiftMask, xK_s), spawn "systemctl suspend")
    -- Quit xmonad
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    -- Restart xmonad
  , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
  , ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
  ] ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
  [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
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
-- gridLayout = defaultSpacing Grid
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
  xmonad $ docks (defaultSettings xMobar)

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
    , keys = myKeys
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
