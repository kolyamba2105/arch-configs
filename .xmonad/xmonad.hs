import Data.Map (fromList)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Tree
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myTerminal = "alacritty"

myNordTerminal = myTerminal ++ " --config-file ~/.config/alacritty/nord-theme.yml"

myFont = "xft:JetBrainsMono Nerd Font:pixelsize=14:antialias=true:hinting=true"

-- Key bindings
--
myKeys =
  [ ("M-S-<Return>", spawn myTerminal),
    -- Launch shell prompt
    ("M-p", shellPrompt myPromptConfig),
    -- Launch firefox
    ("M-f", spawn "firefox"),
    -- Launch firefox in private mode
    ("M-S-f", spawn "firefox --private-window"),
    -- Launch brave
    ("M-g", spawn "brave"),
    -- Launch brave in incognito mode
    ("M-S-g", spawn "brave --incognito"),
    -- Take a screenshot of entire display
    ("M-<Print>", spawn "scrot ~/Pictures/Screenshots/screen-%Y-%m-%d-%H-%M-%S.png -d 1"),
    -- Take a screenshot of focused window
    ("M-C-<Print>", spawn "scrot ~/Pictures/Screenshots/window-%Y-%m-%d-%H-%M-%S.png -d 1-u"),
    -- Take a screenshot of chosen area
    ("M-S-<Print>", spawn "scrot ~/Pictures/Screenshots/area-%Y-%m-%d-%H-%M-%S.png -s 1-u"),
    -- Launch file manager
    ("M-d", spawn (myNordTerminal ++ " -t Ranger -e ranger")),
    -- Launch htop
    ("M-S-t", spawn (myNordTerminal ++ " -t HTOP -e htop")),
    -- Launch PulseMixer
    ("M-s", spawn (myNordTerminal ++ " -t PulseMixer -e pulsemixer")),
    -- Launch Pavucontrol (extended volume control GUI)
    ("M-S-s", spawn "pavucontrol"),
    -- Open terminal ScratchPad
    ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal"),
    -- Close focused window
    ("M-S-c", kill),
    -- Rotate through the available layout algorithms
    ("M-<Space>", sendMessage NextLayout),
    -- Resize viewed windows to the correct size
    ("M-n", refresh),
    -- Move focus to the next window
    ("M-<Tab>", windows W.focusDown),
    -- Move focus to the next window
    ("M-j", windows W.focusDown),
    -- Move focus to the previous window
    ("M-k", windows W.focusUp),
    -- Move focus to the master window
    ("M-m", windows W.focusMaster),
    -- Swap the focused window and the master window
    ("M-<Return>", windows W.swapMaster),
    -- Swap the focused window with the next window
    ("M-S-j", windows W.swapDown),
    -- Swap the focused window with the previous window
    ("M-S-k", windows W.swapUp),
    -- Shrink the master area
    ("M-h", sendMessage Shrink),
    -- Expand the master area
    ("M-l", sendMessage Expand),
    -- Push window back into tiling
    ("M-t", withFocused $ windows . W.sink),
    -- Increment the number of windows in the master area
    ("M-,", sendMessage (IncMasterN 1)),
    -- Decrement the number of windows in the master area
    ("M-.", sendMessage (IncMasterN (-1))),
    -- Increment brightness by 10%
    ("M-<Page_Up>", spawn "xbacklight -inc 10"),
    -- Decrement brightness by 10%
    ("M-<Page_Down>", spawn "xbacklight -dec 10"),
    -- Toggle the status bar gap
    ("M-b", sendMessage ToggleStruts),
    -- Quit xmonad (don't want to accidentally kill session)
    ("M-S-q", return ()),
    -- Restart xmonad
    ("M-q", spawn "xmonad --recompile; xmonad --restart")
  ]

-- Layouts
--
defaultTallLayout = Tall nMaster delta ratio
  where
    nMaster = 1
    delta = 2 / 100
    ratio = 1 / 2

tallLayout = renamed [Replace "Tall"] (defaultSpacing defaultTallLayout)

tabbedLayout = renamed [Replace "Tabbed"] (noBorders (tabbedBottom shrinkText myTabbedTheme))

myTabbedTheme =
  def
    { fontName = myFont,
      activeColor = "#b2ff59",
      inactiveColor = "#424242",
      activeBorderColor = "#b2ff59",
      inactiveBorderColor = "#424242",
      activeTextColor = "#333333",
      inactiveTextColor = "#ffffff"
    }

-- mirrorLayout = renamed [Replace "Mirror"] (defaultSpacing (Mirror defaultTallLayout))

-- gridLayout = renamed [Replace "Grid"] (defaultSpacing Grid)

-- fullLayout = renamed [Replace "Full"] (noBorders Full)

myLayout = avoidStruts (tallLayout ||| tabbedLayout)

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

defaultSpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
defaultSpacing = mySpacing 4

-- Window rules
--

rectCentered50 :: W.RationalRect
rectCentered50 = W.RationalRect 0.25 0.25 0.5 0.5

rectCentered60 :: W.RationalRect
rectCentered60 = W.RationalRect 0.2 0.2 0.6 0.6

rectCentered70 :: W.RationalRect
rectCentered70 = W.RationalRect 0.15 0.15 0.7 0.7

rectCentered80 :: W.RationalRect
rectCentered80 = W.RationalRect 0.1 0.1 0.8 0.8

rectCentered90 :: W.RationalRect
rectCentered90 = W.RationalRect 0.05 0.05 0.9 0.9

rectCentered100 :: W.RationalRect
rectCentered100 = W.RationalRect 0 0 1 1

myManageHook =
  composeAll
    [ title =? "PulseMixer" --> customFloating rectCentered50,
      title =? "Ranger" --> customFloating rectCentered90,
      title =? "HTOP" --> customFloating rectCentered80,
      className =? "Arandr" --> customFloating rectCentered50,
      className =? "Nitrogen" --> customFloating rectCentered50,
      className =? "Pavucontrol" --> customFloating rectCentered50
    ]
    <+> namedScratchpadManageHook myScratchPads

-- Event handling
--
myEventHook = mempty

-- Status bars and logging
--
myLogHook = return ()

-- Startup hook
--
keyboardLayout = "setxkbmap -layout us,pl,ru,ua -option grp:alt_shift_toggle"

typingRepeatSpeed = "xset r rate 200 35"

cursor = "xsetroot -cursor_name left_ptr"

wallpapers = "nitrogen --restore &"

compositor = "picom --config ~/.config/picom/picom.conf &"

myStartupHook = do
  spawn keyboardLayout
  spawn typingRepeatSpeed
  spawn cursor
  spawn wallpapers
  spawn compositor

-- Scratchpads
--
terminalScratchPad :: NamedScratchpad
terminalScratchPad = NS "terminal" spawn find manage
  where
    spawn = myNordTerminal ++ " -t ScratchPad"
    find = title =? "ScratchPad"
    manage = customFloating rectCentered70

myScratchPads :: [NamedScratchpad]
myScratchPads = [terminalScratchPad]

--Prompt config
--
myPromptConfig :: XPConfig
myPromptConfig =
  def
    { font = myFont,
      bgColor = "#333333",
      fgColor = "#e0e0e0",
      bgHLight = "#00b0ff",
      fgHLight = "#333333",
      borderColor = "#00b0ff",
      promptBorderWidth = 2,
      position = CenteredAt 0.4 0.6,
      height = 32,
      maxComplRows = Just 5,
      showCompletionOnTab = True
    }

-- Main
--
main = do
  xMobar <- spawnPipe "xmobar -x 0"
  xmonad $ docks (defaultSettings xMobar `additionalKeysP` myKeys)

xmobarPrettyPrinting xMobar =
  dynamicLogWithPP
    xmobarPP
      { ppOutput = hPutStrLn xMobar,
        ppCurrent = xmobarColor "#b2ff59" "" . wrap "[" "]",
        ppVisible = xmobarColor "#18ffff" "",
        ppHidden = xmobarColor "#40c4ff" "" . wrap "*" "",
        ppHiddenNoWindows = xmobarColor "#ff4081" "",
        ppTitle = xmobarColor "#64ffda" "" . shorten 50,
        ppLayout = xmobarColor "#eeff41" "",
        ppSep = "<fc=#eeeeee> | </fc>"
      }

defaultSettings xMobar =
  def
    { terminal = myTerminal,
      focusFollowsMouse = True,
      clickJustFocuses = False,
      borderWidth = 2,
      modMask = mod4Mask,
      workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
      normalBorderColor = "#333333",
      focusedBorderColor = "#00b0ff",
      layoutHook = myLayout,
      manageHook = manageDocks <+> myManageHook,
      handleEventHook = myEventHook,
      logHook = myLogHook <+> xmobarPrettyPrinting xMobar,
      startupHook = myStartupHook
    }
