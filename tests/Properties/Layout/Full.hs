{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Layout.Full where

import Test.QuickCheck
import Instances


import XMonad.StackSet hiding (filter)
import XMonad.Core
import XMonad.Layout

import qualified Graphics.X11.Xlib.Types as X11Types

import Data.Maybe

------------------------------------------------------------------------
-- Full layout

-- pureLayout works for Full

prop_purelayout_full :: X11Types.Rectangle -> Gen Bool
prop_purelayout_full rect = do
  x <- (arbitrary :: Gen T) `suchThat` (isJust . peek)
  let lay = Full
      st = fromJust . stack . workspace . current $ x
      ts = pureLayout lay rect st
  return $
        length ts == 1        -- only one window to view
      &&
        snd (head ts) == rect -- and sets fullscreen
      &&
        fst (head ts) == fromJust (peek x) -- and the focused window is shown


-- what happens when we send an IncMaster message to Full --- Nothing
prop_sendmsg_full :: NonNegative Int -> Bool
prop_sendmsg_full (NonNegative k) =
         isNothing (Full `pureMessage` (SomeMessage (IncMasterN k)))

prop_desc_full :: Bool
prop_desc_full = description Full == show Full
