{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.

  If you want to customize this file, the easiest workflow goes
  something like this:
    1. Make a small change.
    2. Hit "super-q", which recompiles and restarts xmonad
    3. If there is an error, undo your change and hit "super-q" again to
       get to a stable place again.
    4. Repeat

  Author:     David Brewer
  Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf
-}

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Actions.SpawnOn
import XMonad.Actions.CopyWindow
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Actions.FloatKeys
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W

{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

myModMask :: KeyMask
myModMask = mod4Mask       -- changes the mod key to "super"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"      -- color of focused border

myNormalBorderColor :: String
myNormalBorderColor = "#cccccc"      -- color of inactive border

myBorderWidth :: Dimension
myBorderWidth = 1              -- width of border around windows

myTerminal :: String
myTerminal = "gnome-terminal"   -- which terminal software to use


{-
  Xmobar configuration variables. These settings control the appearance
  of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

myTitleColor :: String
myTitleColor     = "#eeeeee"  -- color of window title

myTitleLength :: Int
myTitleLength    = 80         -- truncate window title to this length

myCurrentWSColor :: String
myCurrentWSColor = "#e6744c"  -- color of active workspace

myVisibleWSColor :: String
myVisibleWSColor = "#c185a7"  -- color of inactive workspace

myUrgentWSColor :: String
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window

myCurrentWSLeft :: String
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight :: String
myCurrentWSRight = "]"

myVisibleWSLeft :: String
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight :: String
myVisibleWSRight = ")"

myUrgentWSLeft :: String
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight :: String
myUrgentWSRight = "}"


{-
  Workspace configuration. Here you can change the names of your
  workspaces. Note that they are organized in a grid corresponding
  to the layout of the number pad.

  I would recommend sticking with relatively brief workspace names
  because they are displayed in the xmobar status bar, where space
  can get tight. Also, the workspace labels are referred to elsewhere
  in the configuration file, so when you change a label you will have
  to find places which refer to it and make a change there as well.

  This central organizational concept of this configuration is that
  the workspaces correspond to keys on the number pad, and that they
  are organized in a grid which also matches the layout of the number pad.
  So, I don't recommend changing the number of workspaces unless you are
  prepared to delve into the workspace navigation keybindings section
  as well.
-}

webWorkspace :: String
webWorkspace = "1:Web"

termWorkspace :: String
termWorkspace = "2:Term"

devWorkspace :: String
devWorkspace = "3:Dev"

chatWorkspace :: String
chatWorkspace = "4:Chat"

misc1Workspace :: String
misc1Workspace = "5:Misc1"

devWebWorkspace :: String
devWebWorkspace = "6:Web2"

mailWorkSpace :: String
mailWorkSpace = "7:Mail"

gimpWorkspace :: String
gimpWorkspace = "9:Pix"

perfWorkspace :: String
perfWorkspace = "-:Perf"

projects :: [Project]
projects = [ Project { projectName      = devWorkspace
                     , projectDirectory = "~/programming"
                     , projectStartHook = Just $ do spawnOn devWorkspace "emacs"}
           , Project { projectName      = webWorkspace
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do spawnOn webWorkspace "chromium-browser"}
           , Project { projectName      = devWebWorkspace
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do spawnOn devWebWorkspace "chromium-browser"}
           , Project { projectName      = termWorkspace
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do spawnOn termWorkspace myTerminal}
           , Project { projectName      = chatWorkspace
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do spawnOn chatWorkspace $ "chromium-browser  --new-window"
                                                      ++ " https://www.messenger.com/ "
                                                      ++ " https://blono-fp-slackers.slack.com/messages/ "
                                                      ++ " https://hangouts.google.com/ "}
           , Project { projectName     = mailWorkSpace
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do spawnOn mailWorkSpace $ "chromium-browser --new-window --app=\"https://inbox.google.com/\""}
           , Project { projectName     = perfWorkspace
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do spawnOn perfWorkspace $ "gnome-system-monitor"}]

myWorkspaces :: [String]
myWorkspaces = [ mailWorkSpace  , "8:Dbg"        , gimpWorkspace
                , chatWorkspace , misc1Workspace , devWebWorkspace
                , webWorkspace  , termWorkspace  , devWorkspace
                , "0:VM"        , perfWorkspace  , "Extr2"]

startupWorkspace :: String
startupWorkspace = webWorkspace

{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.

  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Here we define some layouts which will be assigned to specific
-- workspaces based on the functionality of that workspace.

-- The GIMP layout uses the ThreeColMid layout. The traditional GIMP
-- floating panels approach is a bit of a challenge to handle with xmonad;
-- I find the best solution is to make the image you are working on the
-- master area, and then use this ThreeColMid layout to make the panels
-- tile to the left and right of the image. If you use GIMP 2.8, you
-- can use single-window mode and avoid this issue.
gimpLayout = smartBorders(avoidStruts(ThreeColMid 1 (3/100) (3/4)))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayouts =
  onWorkspace gimpWorkspace gimpLayout
  -- Define group of default layouts used on most screens, in the order they
  -- will appear. "smartBorders" modifier makes it so the borders on windows
  -- only appear if there is more than one visible window. "avoidStruts"
  -- modifier makes it so that the layout provides space for the status bar at
  -- the top of the screen.
  $ smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on the left, and remaining
  -- windows tile on the right. By default each area takes up half the screen,
  -- but you can resize using "super-h" and "super-l".
  ResizableTall 1 (3/100) (1/2) []

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
  -- ||| ThreeColMid 1 (3/100) (3/4)

  -- Circle layout places the master window in the center of the screen.
  -- Remaining windows appear in a circle around it
  -- ||| Circle

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid))


{-
  Custom keybindings. In this section we define a list of relatively
  straightforward keybindings. This would be the clearest place to
  add your own keybindings, or change the keys we have defined
  for certain functions.

  It can be difficult to find a good list of keycodes for use
  in xmonad. I have found this page useful -- just look
  for entries beginning with "xK":

  http://xmonad.org/xmonad-docs/xmonad/doc-index-X.html

  Note that in the example below, the last three entries refer
  to nonstandard keys which do not have names assigned by
  xmonad. That's because they are the volume and mute keys
  on my laptop, a Lenovo W520.

  If you have special keys on your keyboard which you
  want to bind to specific actions, you can use the "xev"
  command-line tool to determine the code for a specific key.
  Launch the command, then type the key in question and watch
  the output.
-}
floatingVideoRR :: W.RationalRect
floatingVideoRR = W.RationalRect ((100-36)/100) (0/1) (36/100) (25/100)

plexCommand :: String
plexCommand = "google-chrome --app=\"http://plex.mjh.io/web/index.html\" --new-window"
spotifyCommand :: String
spotifyCommand = "spotify"

scratchpads :: [NamedScratchpad]
scratchpads = [ (NS "plex" plexCommand (className =? "Google-chrome")
                 (customFloating floatingVideoRR))
              , (NS "spotify" spotifyCommand (className =? "Spotify")
                 (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)))]


moveVertical     x = keysMoveWindow   ( 0      , 10 * x )
resizeVertical   x = keysResizeWindow ( 0      , 10 * x ) (1,1)
moveHorizontal   x = keysMoveWindow   ( 10 * x , 0      )
resizeHorizontal x = keysResizeWindow ( 10 * x , 0      ) (1,1)
resizeCorner     x = keysResizeWindow ( 10 * x , 10 * x ) (1,1)

modC = myModMask  .|. controlMask
modM = myModMask  .|. mod1Mask
modS = myModMask .|. shiftMask

myKeyBindings :: [((KeyMask, KeySym), X ())]
myKeyBindings = [((myModMask, xK_b), sendMessage ToggleStruts)
                , ((myModMask, xK_a), sendMessage MirrorShrink)
                , ((myModMask, xK_z), sendMessage MirrorExpand)
                , ((myModMask, xK_p), namedScratchpadAction scratchpads "plex")
                , ((myModMask, xK_s), namedScratchpadAction scratchpads "spotify")
                , ((myModMask .|. mod1Mask, xK_space), spawn "synapse")
                , ((myModMask, xK_u), focusUrgent)

                , ((modM, xK_c), wsContainingCopies >>= \ws -> case ws of
                                                                 [] -> windows copyToAll
                                                                 _ -> killAllOtherCopies)

                , ((modM, xK_h), withFocused (resizeHorizontal ( 1)))
                , ((modS, xK_h), withFocused (moveHorizontal   (-1)))
                , ((modM, xK_l), withFocused (resizeHorizontal (-1)))
                , ((modS, xK_l), withFocused (moveHorizontal   ( 1)))

                , ((modM, xK_k), withFocused (resizeVertical   ( 1)))
                , ((modS, xK_k), withFocused (moveVertical     ( 1)))
                , ((modM, xK_j), withFocused (resizeVertical   (-1)))
                , ((modS, xK_j), withFocused (moveVertical     (-1)))

                , ((modM, xK_n), withFocused (resizeCorner     (-1)))
                , ((modM, xK_p), withFocused (resizeCorner     ( 1)))


                -- , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
                , ((0, 0x1008FF11), spawn "amixer -q set Master 10%-")
                , ((0, 0x1008FF13), spawn "amixer -q set Master 10%+")
                , ((0, 0x1008FF03), spawn "xbacklight -10")
                , ((0, 0x1008FF02), spawn "xbacklight +10")
                ]


{-
  Management hooks. You can use management hooks to enforce certain
  behaviors when specific programs or windows are launched. This is
  useful if you want certain windows to not be managed by xmonad,
  or sent to a specific workspace, or otherwise handled in a special
  way.

  Each entry within the list of hooks defines a way to identify a
  window (before the arrow), and then how that window should be treated
  (after the arrow).

  To figure out to identify your window, you will need to use a
  command-line tool called "xprop". When you run xprop, your cursor
  will temporarily change to crosshairs; click on the window you
  want to identify. In the output that is printed in your terminal,
  look for a couple of things:
    - WM_CLASS(STRING): values in this list of strings can be compared
      to "className" to match windows.
    - WM_NAME(STRING): this value can be compared to "resource" to match
      windows.

  The className values tend to be generic, and might match any window or
  dialog owned by a particular program. The resource values tend to be
  more specific, and will be different for every dialog. Sometimes you
  might want to compare both className and resource, to make sure you
  are matching only a particular window which belongs to a specific
  program.

  Once you've pinpointed the window you want to manipulate, here are
  a few examples of things you might do with that window:
    - doIgnore: this tells xmonad to completely ignore the window. It will
      not be tiled or floated. Useful for things like launchers and
      trays.
    - doFloat: this tells xmonad to float the window rather than tiling
      it. Handy for things that pop up, take some input, and then go away,
      such as dialogs, calculators, and so on.
    - doF (W.shift "Workspace"): this tells xmonad that when this program
      is launched it should be sent to a specific workspace. Useful
      for keeping specific tasks on specific workspaces. In the example
      below I have specific workspaces for chat, development, and
      editing images.
-}

myManagementHooks :: [ManageHook]
myManagementHooks = [ resource =? "synapse" --> doIgnore
                    , resource =? "stalonetray" --> doIgnore
                    , (className =? "Gimp-2.8") --> doF (W.shift gimpWorkspace)
                    , namedScratchpadManageHook scratchpads]

{-
  Workspace navigation keybindings. This is probably the part of the
  configuration I have spent the most time messing with, but understand
  the least. Be very careful if messing with this section.
-}

-- We define two lists of keycodes for use in the rest of the
-- keyboard configuration. The first is the list of numpad keys,
-- in the order they occur on the keyboard (left to right and
-- top to bottom). The second is the list of number keys, in an
-- order corresponding to the numpad. We will use these to
-- make workspace navigation commands work the same whether you
-- use the numpad or the top-row number keys. And, we also
-- use them to figure out where to go when the user
-- uses the arrow keys.
numKeys :: [KeySym]
numKeys = [ xK_7, xK_8, xK_9
          , xK_4, xK_5, xK_6
          , xK_1, xK_2, xK_3
          , xK_0, xK_minus, xK_equal]

-- Here, some magic occurs that I once grokked but has since
-- fallen out of my head. Essentially what is happening is
-- that we are telling xmonad how to navigate workspaces,
-- how to send windows to different workspaces,
-- and what keys to use to change which monitor is focused.
myKeys :: [((KeyMask, KeySym), X ())]
myKeys = myKeyBindings
  ++ [((m .|. myModMask, k), windows $ f i) |
      (i, k) <- zip myWorkspaces numKeys
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

{-
  Here we actually stitch together all the configuration settings
  and run xmonad. We also spawn an instance of xmobar and pipe
  content into it via the logHook.
-}

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad
    $ dynamicProjects projects
    $ withUrgencyHook NoUrgencyHook $ def
    { focusedBorderColor = myFocusedBorderColor
    , normalBorderColor = myNormalBorderColor
    , terminal = myTerminal
    , borderWidth = myBorderWidth
    , layoutHook = myLayouts
    , workspaces = myWorkspaces
    , modMask = myModMask
    , handleEventHook = fullscreenEventHook
    , startupHook = do setWMName "LG3D"
                       windows $ W.greedyView startupWorkspace
                       spawn "~/.xmonad/startup-hook"
    , manageHook = manageHook def
                   <+> composeAll myManagementHooks
                   <+> manageDocks
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
      , ppCurrent = xmobarColor myCurrentWSColor ""
      , ppLayout = \_ -> ""
      , ppHidden = \_ -> ""
      , ppVisible = \_ -> ""
      , ppUrgent = xmobarColor myUrgentWSColor ""
                   . wrap myUrgentWSLeft myUrgentWSRight
    }
  }
    `additionalKeys` myKeys
