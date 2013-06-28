{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Focus where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.Maybe (fromJust)

-- ---------------------------------------------------------------------
-- rotating focus
--

-- master/focus
--
-- The tiling order, and master window, of a stack is unaffected by focus changes.
--
prop_focus_left_master :: SizedPositive -> T -> Bool
prop_focus_left_master (SizedPositive n) x =
    index (applyN (Just n) focusUp x) == index x

prop_focus_right_master :: SizedPositive -> T -> Bool
prop_focus_right_master (SizedPositive  n) x =
    index (applyN (Just n) focusDown x) == index x

prop_focus_master_master :: SizedPositive -> T -> Bool
prop_focus_master_master (SizedPositive n) x =
    index (applyN (Just n) focusMaster x) == index x

prop_focusWindow_master :: NonNegative Int -> T -> Bool
prop_focusWindow_master (NonNegative n) x =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = n `mod` length s
                   in index (focusWindow (s !! i) x) == index x


-- shifting focus is trivially reversible
prop_focus_left :: T -> Bool
prop_focus_left  x = (focusUp  (focusDown x)) == x

prop_focus_right :: T -> Bool
prop_focus_right x = (focusDown (focusUp  x)) ==  x


-- focus master is idempotent
prop_focusMaster_idem :: T -> Bool
prop_focusMaster_idem x = focusMaster x == focusMaster (focusMaster x)

-- focusWindow actually leaves the window focused...
prop_focusWindow_works :: NonNegative Int -> T -> Bool
prop_focusWindow_works (NonNegative n) x =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = n `mod` length s
                   in (focus . fromJust . stack . workspace . current) (focusWindow (s !! i) x) == (s !! i)

-- rotation through the height of a stack gets us back to the start
prop_focus_all_l :: T -> Bool
prop_focus_all_l x = (foldr (const focusUp) x [1..n]) == x
  where n = length (index x)

prop_focus_all_r :: T -> Bool
prop_focus_all_r x = (foldr (const focusDown) x [1..n]) == x
  where n = length (index x)

-- TODO: Fix this?
-- prop_rotate_all (x :: T) = f (f x) == f x
--     f x' = foldr (\_ y -> rotate GT y) x' [1..n]


-- focus is local to the current workspac
prop_focus_down_local :: T -> Bool
prop_focus_down_local x = hidden_spaces (focusDown x) == hidden_spaces x

prop_focus_up_local :: T -> Bool
prop_focus_up_local x = hidden_spaces (focusUp x) == hidden_spaces x

prop_focus_master_local :: T -> Bool
prop_focus_master_local x = hidden_spaces (focusMaster x) == hidden_spaces x

prop_focusWindow_local :: NonNegative Int -> T -> Bool
prop_focusWindow_local (NonNegative n) x =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = n `mod` length s
                   in hidden_spaces (focusWindow (s !! i) x) == hidden_spaces x

-- On an invalid window, the stackset is unmodified
prop_focusWindow_identity :: T -> Gen Bool
prop_focusWindow_identity x = do
    n <- arbitrary `suchThat` \n' -> not $ n' `member` x
    return $ focusWindow n x == x
