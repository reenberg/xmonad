-----------------------------------------------------------------------------
-- |
-- Module      :  Main.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses mtl, X11, posix
--
-----------------------------------------------------------------------------
--
-- xmonad, a minimalist, tiling window manager for X11
--

import Data.Bits
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State

import System.Environment (getArgs)

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama    (getScreenInfo)

import XMonad
import Config
import StackSet (new, floating, member)
import Operations

--
-- The main entry point
--
main :: IO ()
main = do
    dpy   <- openDisplay ""
    let dflt = defaultScreen dpy
        initcolor c = fst `liftM` allocNamedColor dpy (defaultColormap dpy dflt) c

    rootw  <- rootWindow dpy dflt
    xinesc <- getScreenInfo dpy
    nbc    <- initcolor normalBorderColor
    fbc    <- initcolor focusedBorderColor
    args <- getArgs

    let winset | ("--resume" : s : _) <- args
               , [(x, "")]            <- reads s = x
               | otherwise = new (fromIntegral workspaces) (fromIntegral $ length xinesc)
        safeLayouts = case defaultLayouts of [] -> (full, []); (x:xs) -> (x, xs)
        cf = XConf
            { display       = dpy
            , theRoot       = rootw
            -- fromIntegral needed for X11 versions that use Int instead of CInt.
            , normalBorder  = nbc
            , focusedBorder = fbc
            }
        st = XState
            { windowset     = winset
            , layouts       = M.fromList [(w, safeLayouts) | w <- [0 .. W workspaces - 1]]
            , statusGaps    = take (length xinesc) $ defaultGaps ++ repeat (0,0,0,0)
            , xineScreens   = xinesc }

    xSetErrorHandler -- in C, I'm too lazy to write the binding: dons

    -- setup initial X environment
    sync dpy False
    selectInput dpy rootw $  substructureRedirectMask .|. substructureNotifyMask
                         .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
    grabKeys dpy rootw
    grabButtons dpy rootw

    sync dpy False

    ws <- scan dpy rootw
    allocaXEvent $ \e ->
        runX cf st $ do
            mapM_ manage ws
            -- withWindowSet (io . hPrint stderr) -- uncomment for state logging

            -- main loop, for all you HOF/recursion fans out there.
            forever $ handle =<< io (nextEvent dpy e >> getEvent e)

      where forever a = a >> forever a

-- ---------------------------------------------------------------------
-- IO stuff. Doesn't require any X state
-- Most of these things run only on startup (bar grabkeys)

-- | scan for any initial windows to manage
scan :: Display -> Window -> IO [Window]
scan dpy rootw = do
    (_, _, ws) <- queryTree dpy rootw
    filterM ok ws

  where ok w = do wa <- getWindowAttributes dpy w
                  return $ not (wa_override_redirect wa)
                         && wa_map_state wa == waIsViewable

-- | Grab the keys back
grabKeys :: Display -> Window -> IO ()
grabKeys dpy rootw = do
    ungrabKey dpy anyKey anyModifier rootw
    forM_ (M.keys keys) $ \(mask,sym) -> do
         kc <- keysymToKeycode dpy sym
         -- "If the specified KeySym is not defined for any KeyCode,
         -- XKeysymToKeycode() returns zero."
         when (kc /= '\0') $ mapM_ (grab kc . (mask .|.)) extraModifiers

  where grab kc m = grabKey dpy kc m rootw True grabModeAsync grabModeAsync

grabButtons :: Display -> Window -> IO ()
grabButtons dpy rootw = do
    ungrabButton dpy anyButton anyModifier rootw
    mapM_ (\(m,b) -> mapM_ (grab b . (m .|.)) extraModifiers) (M.keys mouseBindings)
  where grab button mask = grabButton dpy button mask rootw False buttonPressMask
                                      grabModeAsync grabModeSync none none

extraModifiers :: [KeyMask]
extraModifiers = [0, numlockMask, lockMask, numlockMask .|. lockMask ]

cleanMask :: KeyMask -> KeyMask
cleanMask = (complement (numlockMask .|. lockMask) .&.)

-- ---------------------------------------------------------------------
-- | Event handler. Map X events onto calls into Operations.hs, which
-- modify our internal model of the window manager state.
--
-- Events dwm handles that we don't:
--
--    [ButtonPress]    = buttonpress,
--    [Expose]         = expose,
--    [PropertyNotify] = propertynotify,
--

handle :: Event -> X ()

-- run window manager command
handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyPress = withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        whenJust (M.lookup (cleanMask m,s) keys) id

-- manage a new window
handle (MapRequestEvent    {ev_window = w}) = withDisplay $ \dpy -> do
    wa <- io $ getWindowAttributes dpy w -- ignore override windows
    -- need to ignore mapping requests by managed windows not on the current workspace
    managed <- isClient w
    when (not (wa_override_redirect wa) && not managed) $ do manage w

-- window destroyed, unmanage it
-- window gone,      unmanage it
handle (DestroyWindowEvent {ev_window = w}) = whenX (isClient w) $ unmanage w

-- We only handle synthetic unmap events, because real events are confusable
-- with the events produced by 'hide'.  ICCCM says that all clients should send
-- synthetic unmap events immediately after unmapping, and later describes
-- clients that do not follow the rule as "obsolete".  For now, we make the
-- simplifying assumption that nobody uses clients that were already obsolete
-- in 1994.  Note that many alternative methods for resolving the hide/withdraw
-- ambiguity are racy.

handle (UnmapEvent         {ev_window = w, ev_send_event = True}) = whenX (isClient w) $ unmanage w

-- set keyboard mapping
handle e@(MappingNotifyEvent {ev_window = w}) = do
    io $ refreshKeyboardMapping e
    when (ev_request e == mappingKeyboard) $ withDisplay $ io . flip grabKeys w

-- click on an unfocused window, makes it focused on this workspace
handle e@(ButtonEvent {ev_window = w,ev_event_type = t,ev_button = b })
    | t == buttonPress = do
    isr <- isRoot w
    if isr then whenJust (M.lookup (cleanMask (ev_state e), b) mouseBindings) ($ ev_subwindow e)
           else focus w
    -- If it's the root window, then it's something we
    -- grabbed in grabButtons. Otherwise, it's click-to-focus.

-- entered a normal window, makes this focused.
handle e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode   e == notifyNormal
                       && ev_detail e /= notifyInferior = focus w

-- left a window, check if we need to focus root
handle e@(CrossingEvent {ev_event_type = t})
    | t == leaveNotify
    = do rootw <- asks theRoot
         when (ev_window e == rootw && not (ev_same_screen e)) $ setFocusX rootw

-- configure a window
handle e@(ConfigureRequestEvent {ev_window = w}) = withDisplay $ \dpy -> do
    ws <- gets windowset
    wa <- io $ getWindowAttributes dpy w

    if M.member w (floating ws) || not (member w ws)
        then do io $ configureWindow dpy w (ev_value_mask e) $ WindowChanges
                    { wc_x            = ev_x e
                    , wc_y            = ev_y e
                    , wc_width        = ev_width e
                    , wc_height       = ev_height e
                    , wc_border_width = fromIntegral borderWidth
                    , wc_sibling      = ev_above e
                    , wc_stack_mode   = ev_detail e }
                when (member w ws) (float w)
        else io $ allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev w w
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width e) none (wa_override_redirect wa)
                 sendEvent dpy w False 0 ev
    io $ sync dpy False

-- the root may have configured
handle (ConfigureEvent {ev_window = w}) = whenX (isRoot w) rescreen

handle _ = return () -- trace (eventName e) -- ignoring
