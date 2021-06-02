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
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Hooks.DynamicLog
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

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

ignoredWorkspaces = ["NSP"]

-- Key bindings
myKeys = coreKeys ++ scratchPadKeys ++ controlKeys ++ cycleWSKeys ++ dynamicWSGroupKeys
  where
    coreKeys =
      [ ("M-C-<Return>", spawn myTransparentTerminal),
        ("M-S-<Return>", spawn myTerminal),
        ("M-S-f", spawn "firefox --private-window"),
        ("M-S-g", spawn "brave --incognito"),
        ("M-f", spawn "firefox"),
        ("M-g", spawn "brave"),
        ("M-p", shellPrompt myPromptConfig)
      ]

    scratchPadKeys =
      [ ("M-M1-<Return>", openScratchPad "terminal"),
        ("M-S-a", openScratchPad "telegram"),
        ("M-S-s", openScratchPad "slack"),
        ("M-S-t", openScratchPad "htop"),
        ("M-d", openScratchPad "ranger"),
        ("M-s", openScratchPad "mixer")
      ]

    controlKeys =
      [ ("<XF86AudioMicMute>", spawn "pactl set-source-mute 1 toggle"),
        ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle"),
        ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10"),
        ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10"),
        ("M-<Page_Down>", spawn "xbacklight -dec 10"),
        ("M-<Page_Up>", spawn "xbacklight -inc 10"),
        ("M-<Print>", spawn "scrot -q 100 ~/Pictures/Screenshots/screen-%Y-%m-%d-%H-%M-%S.png"),
        ("M-C-<Print>", spawn "scrot -u -q 100 ~/Pictures/Screenshots/window-%Y-%m-%d-%H-%M-%S.png"),
        ("M-S-<Print>", spawn "scrot -s -q 100 ~/Pictures/Screenshots/area-%Y-%m-%d-%H-%M-%S.png"),
        ("M-S-l", spawn "slock"),
        ("M-b", sendMessage ToggleStruts)
      ]

    cycleWSKeys =
      [ ("M-C-<Tab>", toggleWS),
        ("M-C-S-h", C.shiftPrevScreen),
        ("M-C-S-j", C.shiftToNext),
        ("M-C-S-k", C.shiftToPrev),
        ("M-C-S-l", C.shiftNextScreen),
        ("M-C-h", C.prevScreen),
        ("M-C-j", nextWS),
        ("M-C-k", prevWS),
        ("M-C-l", C.nextScreen)
      ]

    dynamicWSGroupKeys =
      [ ("M-M1-f", forgetGroup),
        ("M-M1-g", goToGroup),
        ("M-M1-n", addGroup)
      ]

myRemovedKeys :: [String]
myRemovedKeys =
  [ "M-S-q"
  ]

myKeysConfig :: XConfig a -> XConfig a
myKeysConfig config = config `additionalKeysP` myKeys `removeKeysP` myRemovedKeys

-- Layouts
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
rectCentered :: Rational -> W.RationalRect
rectCentered percentage = W.RationalRect offset offset percentage percentage
  where
    offset = (1 - percentage) / 2

vertRectCentered :: Rational -> W.RationalRect
vertRectCentered height = W.RationalRect offsetX offsetY width height
  where
    width = height / 2
    offsetX = (1 - width) / 2
    offsetY = (1 - height) / 2

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
      className =? "Pavucontrol" --> customFloating (rectCentered 0.5)
    ]
    <+> namedScratchpadManageHook myScratchPads

-- Event handling
myEventHook = mempty

-- Status bars and logging
myLogHook = return ()

-- Startup hook
keyboardLayout = "setxkbmap -layout us,pl,ru,ua -option grp:alt_shift_toggle"

typingRepeatSpeed = "xset r rate 180 40"

cursor = "xsetroot -cursor_name left_ptr"

wallpapers = "~/.fehbg &"

compositor = "picom --config ~/.config/picom/picom.conf &"

notificationDaemon = "dunst"

myStartupHook = do
  spawn keyboardLayout
  spawn typingRepeatSpeed
  spawn cursor
  spawn wallpapers
  spawn compositor
  spawn notificationDaemon

-- Scratchpads
terminalScratchPad :: NamedScratchpad
terminalScratchPad = NS "terminal" spawn find manage
  where
    spawn = myTerminal ++ " -t Terminal"
    find = title =? "Terminal"
    manage = customFloating $ vertRectCentered 0.9

rangerScratchPad :: NamedScratchpad
rangerScratchPad = NS "ranger" spawn find manage
  where
    spawn = myTerminal ++ " -t Ranger -e ranger"
    find = rangerWindowQuery
    manage = nonFloating

htopScratchPad :: NamedScratchpad
htopScratchPad = NS "htop" spawn find manage
  where
    spawn = myTerminal ++ " -t HTOP -e htop"
    find = htopWindowQuery
    manage = customFloating $ rectCentered 0.8

mixerScratchPad :: NamedScratchpad
mixerScratchPad = NS "mixer" spawn find manage
  where
    spawn = myTerminal ++ " -t PulseMixer -e pulsemixer"
    find = pulseMixerWindowQuery
    manage = customFloating $ rectCentered 0.5

slackScratchPad :: NamedScratchpad
slackScratchPad = NS "slack" spawn find manage
  where
    spawn = "slack"
    find = className =? "Slack"
    manage = nonFloating

telegramScratchPad :: NamedScratchpad
telegramScratchPad = NS "telegram" spawn find manage
  where
    spawn = "telegram-desktop"
    find = className =? "TelegramDesktop"
    manage = nonFloating

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ htopScratchPad,
    mixerScratchPad,
    rangerScratchPad,
    slackScratchPad,
    telegramScratchPad,
    terminalScratchPad
  ]

openScratchPad :: String -> X ()
openScratchPad = namedScratchpadAction myScratchPads

--Prompt config
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

-- Dynamic workspace groups
addGroup :: X ()
addGroup = promptWSGroupAdd myPromptConfig "Name group: "

goToGroup :: X ()
goToGroup = promptWSGroupView myPromptConfig "Go to group: "

forgetGroup :: X ()
forgetGroup = promptWSGroupForget myPromptConfig "Forget group: "

-- CycleWS
workspaceType :: C.WSType
workspaceType = C.WSIs $ return (\(W.Workspace tag _ stack) -> isJust stack && tag `notElem` ignoredWorkspaces)

moveTo :: Direction1D -> X ()
moveTo direction = C.moveTo direction workspaceType

nextWS :: X ()
nextWS = moveTo Next

prevWS :: X ()
prevWS = moveTo Prev

toggleWS :: X ()
toggleWS = C.toggleWS' ignoredWorkspaces

-- Main
main :: IO ()
main = do
  xMobar <- spawnPipe "xmobar -x 0"
  xmonad $ docks (defaultSettings xMobar & myKeysConfig)

xmobarPrettyPrinting :: Handle -> X ()
xmobarPrettyPrinting xMobar =
  (dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP)
    xmobarPP
      { ppCurrent = xmobarColor' 4 . wrap "[" "]",
        ppExtras = [windowCount],
        ppHidden = xmobarColor' 13 . wrap "-" "-",
        ppHiddenNoWindows = xmobarColor' 8,
        ppLayout = xmobarColor' 4,
        ppOutput = hPutStrLn xMobar,
        ppSep = "<fc=" ++ (colorPalette !! 4) ++ "> " ++ "\63196" ++ " </fc>",
        ppTitle = xmobarColor' 4 . shorten 50,
        ppUrgent = xmobarColor' 11 . wrap "!" "!",
        ppVisible = xmobarColor' 14 . wrap "<" ">"
      }

xmobarColor' :: Int -> String -> String
xmobarColor' i = xmobarColor (colorPalette !! i) ""

windowCount :: X (Maybe String)
windowCount =
  gets $
    fmap ("\62600  " ++)
      . Just
      . show
      . length
      . W.integrate'
      . W.stack
      . W.workspace
      . W.current
      . windowset

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
