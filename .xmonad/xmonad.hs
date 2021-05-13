import Control.Monad (liftM2)
import Data.Function
import Data.Map (fromList)
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Tree
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import qualified XMonad.Actions.CycleWS as C
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
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

myTransparentTerminal = myTerminal ++ " -o background_opacity=0.9"

myFont = "xft:JetBrainsMono Nerd Font:pixelsize=14:antialias=true:hinting=true"

nonVisibleWorkspaces = ["NSP"]

nonReachableWorkspaces = ["FM"]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] ++ nonReachableWorkspaces ++ nonVisibleWorkspaces

-- Key bindings
--
myKeys =
  [ ("M-S-<Return>", spawn myTerminal),
    -- Launch terminal with transparent background
    ("M-C-<Return>", spawn myTransparentTerminal),
    -- Open terminal ScratchPad
    ("M-S-n", namedScratchpadAction myScratchPads "terminal"),
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
    -- Launch file manager
    ("M-d", toggleFileManager),
    -- Launch htop
    ("M-S-t", raiseMaybe (runInTerm "-t HTOP" "htop") htopWindowQuery),
    -- Launch PulseMixer
    ("M-s", raiseMaybe (runInTerm "-t PulseMixer" "pulsemixer") pulseMixerWindowQuery),
    -- Launch Pavucontrol (extended volume control GUI)
    ("M-S-s", spawn "pavucontrol"),
    -- Take a screenshot of entire display
    ("M-<Print>", spawn "scrot -q 100 ~/Pictures/Screenshots/screen-%Y-%m-%d-%H-%M-%S.png"),
    -- Take a screenshot of focused window
    ("M-C-<Print>", spawn "scrot -u -q 100 ~/Pictures/Screenshots/window-%Y-%m-%d-%H-%M-%S.png"),
    -- Take a screenshot of chosen area
    ("M-S-<Print>", spawn "scrot -s -q 100 ~/Pictures/Screenshots/area-%Y-%m-%d-%H-%M-%S.png"),
    -- Increment brightness by 10%
    ("M-<Page_Up>", spawn "xbacklight -inc 10"),
    -- Decrement brightness by 10%
    ("M-<Page_Down>", spawn "xbacklight -dec 10"),
    -- Switch to the next workspace
    ("M-C-j", nextWS),
    -- Switch to the previous workspace
    ("M-C-k", prevWS),
    -- Move the focused window to the next workspace
    ("M-C-S-j", C.shiftToNext),
    -- Move the focused window to the previous workspace
    ("M-C-S-k", C.shiftToPrev),
    -- View next screen
    ("M-C-l", C.nextScreen),
    -- View prev screen
    ("M-C-h", C.prevScreen),
    -- Move focused window to workspace on next screen
    ("M-C-S-l", C.shiftNextScreen),
    -- Move focused window to workspace on prev screen
    ("M-C-S-h", C.shiftPrevScreen),
    -- Toggle to the workspace displayed previously
    ("M-C-<Tab>", toggleWS),
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
    -- Toggle the status bar gap
    ("M-b", sendMessage ToggleStruts),
    -- Quit xmonad (don't want to accidentally kill session)
    ("M-S-q", return ()),
    -- Restart xmonad
    ("M-q", spawn "xmonad --recompile; xmonad --restart")
  ]

-- Layouts
--
defaultTall = Tall 1 0.05 0.5

defaultLayout = renamed [Replace "Default"] $ defaultSpacing defaultTall

tabbedLayout = renamed [Replace "Tabbed"] $ noBorders $ tabbedBottom shrinkText myTabbedTheme

myTabbedTheme =
  def
    { fontName = myFont,
      activeColor = colorPalette !! 8,
      inactiveColor = colorPalette !! 1,
      activeBorderColor = colorPalette !! 8,
      inactiveBorderColor = colorPalette !! 1,
      activeTextColor = colorPalette !! 1,
      inactiveTextColor = colorPalette !! 4
    }

mirrorLayout = renamed [Replace "Mirror"] $ defaultSpacing $ Mirror defaultTall

gridLayout = renamed [Replace "Grid"] $ defaultSpacing Grid

monocleLayout = renamed [Replace "Monocle"] $ noBorders Full

myLayout = avoidStruts $ defaultLayout ||| monocleLayout

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

defaultSpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
defaultSpacing = mySpacing 4

-- Window rules
--

rectCentered :: Rational -> W.RationalRect
rectCentered percentage = W.RationalRect offset offset percentage percentage
  where
    offset = (1 - percentage) / 2

viewShift :: WorkspaceId -> Query (Endo WindowSet)
viewShift = doF . liftM2 (.) W.greedyView W.shift

htopWindowQuery :: Query Bool
htopWindowQuery = title =? "HTOP"

pulseMixerWindowQuery :: Query Bool
pulseMixerWindowQuery = title =? "PulseMixer"

rangerWindowQuery :: Query Bool
rangerWindowQuery = title =? "Ranger"

myManageHook =
  composeAll
    [ className =? "Arandr" --> customFloating (rectCentered 0.5),
      className =? "Pavucontrol" --> customFloating (rectCentered 0.5),
      htopWindowQuery --> customFloating (rectCentered 0.8),
      pulseMixerWindowQuery --> customFloating (rectCentered 0.5),
      rangerWindowQuery --> viewShift "FM"
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

wallpapers = "~/.fehbg &"

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
    spawn = myTerminal ++ " -t ScratchPad"
    find = title =? "ScratchPad"
    manage = customFloating (rectCentered 0.7)

myScratchPads :: [NamedScratchpad]
myScratchPads = [terminalScratchPad]

--Prompt config
--
myPromptConfig :: XPConfig
myPromptConfig =
  def
    { font = myFont,
      bgColor = colorPalette !! 1,
      fgColor = colorPalette !! 4,
      bgHLight = colorPalette !! 8,
      fgHLight = colorPalette !! 2,
      promptBorderWidth = 0,
      position = CenteredAt 0.4 0.5,
      height = 40,
      maxComplRows = Just 5,
      showCompletionOnTab = True
    }

-- CycleWS
--
workspaceType :: C.WSType
workspaceType = C.WSIs $ return (\(W.Workspace tag _ stack) -> isJust stack && tag `notElem` ignoredWorkspaces)
  where
    ignoredWorkspaces = nonReachableWorkspaces ++ nonVisibleWorkspaces

moveTo :: Direction1D -> X ()
moveTo direction = C.moveTo direction workspaceType

nextWS :: X ()
nextWS = moveTo Next

prevWS :: X ()
prevWS = moveTo Prev

toggleWS :: X ()
toggleWS = C.toggleWS' $ nonReachableWorkspaces ++ nonVisibleWorkspaces

toggleOrView :: WorkspaceId -> X ()
toggleOrView = C.toggleOrDoSkip nonVisibleWorkspaces W.greedyView

-- If there are windows with title Ranger,
-- then toggle the FM workspace (if it's already opened,
-- then go back to previously opened workspace, otherwise go to "FM")
-- otherwise open Ranger (and ManageHook will shift you to "FM" workspace)
toggleFileManager :: X ()
toggleFileManager = ifWindows rangerWindowQuery (const toggle) open
  where
    toggle = toggleOrView "FM"
    open = runInTerm "-t Ranger" "ranger"

-- Main
--
main :: IO ()
main = do
  xMobar <- spawnPipe "xmobar -x 0"
  xmonad $ docks (defaultSettings xMobar `additionalKeysP` myKeys)

xmobarPrettyPrinting :: Handle -> X ()
xmobarPrettyPrinting xMobar =
  (dynamicLogWithPP . filterOutNonVisibleWorkspacesPP)
    xmobarPP
      { ppCurrent = xmobarColor' 4 . wrap "[" "]",
        ppExtras = [windowCount],
        ppHidden = xmobarColor' 13 . wrap "-" "-",
        ppHiddenNoWindows = xmobarColor' 8,
        ppLayout = xmobarColor' 4,
        ppOutput = hPutStrLn xMobar,
        ppSep = "<fc=" ++ (colorPalette !! 4) ++ "> | </fc>",
        ppTitle = xmobarColor' 4 . shorten 50,
        ppVisible = xmobarColor' 14 . wrap "<" ">"
      }

xmobarColor' :: Int -> String -> String
xmobarColor' i = xmobarColor (colorPalette !! i) ""

filterOutNonVisibleWorkspaces :: [WindowSpace] -> [WindowSpace]
filterOutNonVisibleWorkspaces = filter (\(W.Workspace tag _ _) -> tag `notElem` nonVisibleWorkspaces)

filterOutNonVisibleWorkspacesPP :: PP -> PP
filterOutNonVisibleWorkspacesPP pp = pp {ppSort = (. filterOutNonVisibleWorkspaces) <$> ppSort pp}

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

defaultSettings xMobar =
  def
    { borderWidth = 2,
      clickJustFocuses = False,
      focusFollowsMouse = True,
      focusedBorderColor = colorPalette !! 6,
      handleEventHook = myEventHook,
      layoutHook = myLayout,
      logHook = myLogHook <+> xmobarPrettyPrinting xMobar,
      manageHook = manageDocks <+> myManageHook,
      modMask = mod4Mask,
      normalBorderColor = head colorPalette,
      startupHook = myStartupHook,
      terminal = myTerminal,
      workspaces = myWorkspaces
    }

-- Nord color palette
-- Each color index corresponds to color index from documentation
-- https://www.nordtheme.com/docs/colors-and-palettes
colorPalette :: [String]
colorPalette =
  [ "#2e3440",
    "#3b4252",
    "#434c5e",
    "#4c566a",
    "#d8dee9",
    "#e5e9f0",
    "#eceff4",
    "#8fbcbb",
    "#88c0d0",
    "#81a1c1",
    "#5e81ac",
    "#bf616a",
    "#d08770",
    "#ebcb8b",
    "#a3be8c",
    "#b48ead"
  ]
