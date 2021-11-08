import           Control.Monad
import           Data.Map
import           Data.Maybe
import           Data.Monoid
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import qualified XMonad.Actions.CycleWS                as C
import           XMonad.Actions.DynamicWorkspaceGroups
import           XMonad.Actions.NoBorders
import           XMonad.Actions.WithAll
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet                       as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

myTerminal = "alacritty"

myFont = "xft:Iosevka Nerd Font:weight=regular:pixelsize=16:antialias=true:hinting=true"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

ignoredWorkspaces = ["NSP"]

-- Key bindings
myKeys =
  coreKeys
    ++ controlKeys
    ++ cycleWSKeys
    ++ dynamicWSGroupKeys
    ++ layoutKeys
    ++ scratchPadKeys
    ++ screenLayoutKeys
    ++ wmKeys
  where
    coreKeys =
      [ ("M-S-<Return>", spawn myTerminal),
        ("M-S-f", spawn "firefox --private-window"),
        ("M-S-g", spawn "brave --incognito"),
        ("M-f", spawn "firefox"),
        ("M-g", spawn "brave"),
        ("M-p", shellPrompt myPromptConfig)
      ]

    controlKeys =
      [ ("<XF86AudioMicMute>", spawn "pactl set-source-mute 1 toggle"),
        ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle"),
        ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10"),
        ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10"),
        ("M-<Print>", spawn "scrot -q 100 ~/Pictures/Screenshots/screen-%Y-%m-%d-%H-%M-%S.png"),
        ("M-C-<Print>", spawn "scrot -u -q 100 ~/Pictures/Screenshots/window-%Y-%m-%d-%H-%M-%S.png"),
        ("M-S-<Print>", spawn "scrot -s -q 100 ~/Pictures/Screenshots/area-%Y-%m-%d-%H-%M-%S.png")
      ]

    cycleWSKeys =
      [ ("M-C-<Tab>", toggleWS),
        ("M-C-h", C.prevScreen),
        ("M-C-j", nextWS),
        ("M-C-k", prevWS),
        ("M-C-l", C.nextScreen),
        ("M-M1-h", C.shiftPrevScreen),
        ("M-M1-j", shiftToNext),
        ("M-M1-k", shiftToPrev),
        ("M-M1-l", C.shiftNextScreen)
      ]

    scratchPadKeys =
      [ ("M-`", openScratchPad "terminal"),
        ("M-, 1", openScratchPad "htop"),
        ("M-, 2", openScratchPad "mixer"),
        ("M-, 3", openScratchPad "ranger"),
        ("M-, 4", openScratchPad "telegram")
      ]

    dynamicWSGroupKeys =
      [ ("M-. 1", viewWSGroup "1"),
        ("M-. 2", viewWSGroup "2"),
        ("M-. 3", viewWSGroup "3"),
        ("M-. 4", viewWSGroup "4")
      ]

    screenLayoutKeys =
      [ ("M-\\ 1", spawn "~/.screenlayout/1-laptop.sh" <!> notification "Laptop"),
        ("M-\\ 2", spawn "~/.screenlayout/2-monitor.sh" <!> notification "Monitor"),
        ("M-\\ 3", spawn "~/.screenlayout/3-dual-monitor.sh" <!> notification "Dual monitor")
      ]
      where
        notification msg = Message Normal "Screen layout" msg

    layoutKeys =
      [ ("M-t", withFocused $ toggleFloat $ vertRectCentered 0.9),
        ("M-S-t", withFocused $ toggleFloat $ rectCentered 0.9)
      ]

    wmKeys =
      [ ("M-M1-c", killAll <!> Message Critical "XMonad" "Killed them all!"),
        ("M-q", spawn "xmonad --recompile && xmonad --restart" <!> Message Normal "XMonad" "Recompiled and restarted!"),
        ("M-[", sendMessage (IncMasterN 1)),
        ("M-]", sendMessage (IncMasterN (-1)))
      ]

myRemovedKeys :: [String]
myRemovedKeys = ["M-b", "M-S-q"]

myKeysConfig :: XConfig a -> XConfig a
myKeysConfig config = config `additionalKeysP` myKeys `removeKeysP` myRemovedKeys

-- Send notification
data UrgencyLevel = Low | Normal | Critical

instance Show UrgencyLevel where
  show Low      = "low"
  show Normal   = "normal"
  show Critical = "critical"

data Notification
  = Message UrgencyLevel String String
  | Command UrgencyLevel String String

wrapInQuotes, wrapIntoCommand :: String -> String
wrapInQuotes = wrap "'" "'"
wrapIntoCommand = wrap "$(" ")"

sendNotification :: Notification -> X ()
sendNotification (Message uLevel summary body) = spawn ("notify-send " ++ wrapInQuotes summary ++ " " ++ wrapInQuotes body ++ " -u " ++ wrapInQuotes (show uLevel))
sendNotification (Command uLevel summary body) = spawn ("notify-send " ++ wrapInQuotes summary ++ " " ++ wrapIntoCommand body ++ " -u " ++ wrapInQuotes (show uLevel))

(<!>) :: X () -> Notification -> X ()
(<!>) action notification = action >> sendNotification notification

-- Layouts
defaultTall = Tall 1 0.05 0.5

tall = renamed [Replace "Default"] $ limitWindows 6 $ defaultSpacing defaultTall

monocle = renamed [Replace "Monocle"] $ defaultSpacing Full

fullScreen = renamed [Replace "FullScreen"] $ noBorders Full

myLayout = avoidStruts $ tall ||| monocle

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

defaultSpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
defaultSpacing = mySpacing 4

toggleFloat :: W.RationalRect -> Window -> X ()
toggleFloat r w = windows (\s -> if member w (W.floating s) then W.sink w s else W.float w r s)

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
    <+> manageDocks

-- Startup hook
myStartupHook = do
  spawn "dunst"
  spawn "setxkbmap -layout us,pl,ru,ua -option grp:alt_shift_toggle"
  spawn "xset r rate 180 40"
  spawn "setxkbmap -option caps:swapescape"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "~/.fehbg &"
  initWorkspaceGroups

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [htop, mixer, ranger, terminal, telegram]
  where
    terminal = NS "terminal" spawn find manage
      where
        spawn = myTerminal ++ " -t Terminal"
        find = title =? "Terminal"
        manage = customFloating $ rectCentered 0.7

    ranger = NS "ranger" spawn find manage
      where
        spawn = myTerminal ++ " -t Ranger -e ranger"
        find = rangerWindowQuery
        manage = customFloating $ rectCentered 0.9

    htop = NS "htop" spawn find manage
      where
        spawn = myTerminal ++ " -t HTOP -e htop"
        find = htopWindowQuery
        manage = customFloating $ rectCentered 0.8

    mixer = NS "mixer" spawn find manage
      where
        spawn = myTerminal ++ " -t PulseMixer -e pulsemixer"
        find = pulseMixerWindowQuery
        manage = customFloating $ rectCentered 0.5

    telegram = NS "telegram" spawn find manage
      where
        spawn = "telegram-desktop"
        find = className =? "TelegramDesktop"
        manage = customFloating $ vertRectCentered 0.9

openScratchPad :: String -> X ()
openScratchPad = namedScratchpadAction myScratchPads

-- Prompt config
myPromptConfig :: XPConfig
myPromptConfig =
  def
    { font = myFont,
      bgColor = background $ primary colors,
      fgColor = foreground $ primary colors,
      bgHLight = yellow $ normal colors,
      promptBorderWidth = 0,
      position = Top,
      height = 28,
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

initWorkspaceGroups :: X ()
initWorkspaceGroups = do
  addRawWSGroup "1" [(S 1, "2"), (S 0, "1")]
  addRawWSGroup "2" [(S 1, "4"), (S 0, "3")]
  addRawWSGroup "3" [(S 1, "6"), (S 0, "5")]
  addRawWSGroup "4" [(S 1, "8"), (S 0, "7")]

-- CycleWS
moveTo :: Direction1D -> X ()
moveTo direction = C.moveTo direction wsType
  where
    wsType = C.WSIs $ return (\(W.Workspace tag _ stack) -> isJust stack && tag `notElem` ignoredWorkspaces)

nextWS :: X ()
nextWS = moveTo Next

prevWS :: X ()
prevWS = moveTo Prev

toggleWS :: X ()
toggleWS = C.toggleWS' ignoredWorkspaces

shiftTo :: Direction1D -> X ()
shiftTo direction = C.shiftTo direction wsType
  where
    wsType = C.WSIs $ return (\(W.Workspace tag _ _) -> tag `notElem` ignoredWorkspaces)

shiftToNext :: X ()
shiftToNext = shiftTo Next

shiftToPrev :: X ()
shiftToPrev = shiftTo Prev

-- Main
main :: IO ()
main = xmonad . withSB statusBar . docks . myKeysConfig $ defaultSettings

statusBar :: StatusBarConfig
statusBar = statusBarProp "xmobar ~/.xmonad/xmobar.config" $ pure pp
  where
    pp = filterOutWsPP ignoredWorkspaces xmobarPP
      { ppCurrent = xmobarColor' (green $ normal colors) . wrap "[" "]",
        ppExtras = [windowCount],
        ppHidden = xmobarColor' (magenta $ normal colors) . wrap "-" "-",
        ppHiddenNoWindows = xmobarColor' (blue $ normal colors),
        ppLayout = ("\57924  " ++),
        ppOrder = \(ws : layout : current : extras) -> [ws, layout] ++ extras ++ [current],
        ppSep = "  ",
        ppTitle = xmobarColor' (green $ normal colors) . shorten 50,
        ppUrgent = xmobarColor' (red $ normal colors) . wrap "!" "!",
        ppVisible = xmobarColor' (yellow $ normal colors) . wrap "<" ">"
      }

xmobarColor' :: String -> String -> String
xmobarColor' color = xmobarColor color ""

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

defaultSettings =
  def
    { borderWidth = 2,
      clickJustFocuses = False,
      focusFollowsMouse = True,
      focusedBorderColor = white $ normal colors,
      handleEventHook = mempty,
      layoutHook = myLayout,
      manageHook = myManageHook,
      modMask = mod4Mask,
      normalBorderColor = black $ normal colors,
      startupHook = myStartupHook,
      terminal = myTerminal,
      workspaces = myWorkspaces
    }

-- Color palette (GruvBox Material)
colors :: Colors
colors = Colors {
  primary = PrimaryColors {
    background = "#282828",
    foreground = "#dfbf8e"
  },
  normal = RegularColors {
    black   = "#665c54",
    blue    = "#7daea3",
    cyan    = "#89b482",
    green   = "#a9b665",
    magenta = "#d3869b",
    red     = "#ea6962",
    white   = "#dfbf8e",
    yellow  = "#e78a4e"
  }
}

data Colors = Colors
  { primary :: PrimaryColors,
    normal  :: RegularColors
  }

data PrimaryColors = PrimaryColors
  { background :: String,
    foreground :: String
  }

data RegularColors = RegularColors
  { black   :: String,
    blue    :: String,
    cyan    :: String,
    green   :: String,
    magenta :: String,
    red     :: String,
    white   :: String,
    yellow  :: String
  }
