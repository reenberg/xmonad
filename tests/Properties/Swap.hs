{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Swap where

--import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

-- ---------------------------------------------------------------------
-- swapUp, swapDown, swapMaster: reordiring windows

-- swap is trivially reversible
prop_swap_left :: T -> Bool
prop_swap_left x = (swapUp  (swapDown x)) == x

prop_swap_right :: T -> Bool
prop_swap_right x = (swapDown (swapUp  x)) ==  x

-- TODO swap is reversible
-- swap is reversible, but involves moving focus back the window with
-- master on it. easy to do with a mouse...
{-
prop_promote_reversible x b = (not . null . fromMaybe [] . flip index x . current $ x) ==>
                            (raiseFocus y . promote . raiseFocus z . promote) x == x
  where _            = x :: T
        dir          = if b then LT else GT
        (Just y)     = peek x
        (Just (z:_)) = flip index x . current $ x
-}

-- swap doesn't change focus
prop_swap_master_focus :: T -> Bool
prop_swap_master_focus x = peek x == (peek $ swapMaster x)
--    = case peek x of
--        Nothing -> True
--        Just f  -> focus (stack (workspace $ current (swap x))) == f

prop_swap_left_focus :: T -> Bool
prop_swap_left_focus x = peek x == (peek $ swapUp   x)

prop_swap_right_focus :: T -> Bool
prop_swap_right_focus x = peek x == (peek $ swapDown  x)

-- swap is local
prop_swap_master_local :: T -> Bool
prop_swap_master_local x = hidden_spaces x == hidden_spaces (swapMaster x)

prop_swap_left_local :: T -> Bool
prop_swap_left_local x = hidden_spaces x == hidden_spaces (swapUp   x)

prop_swap_right_local :: T -> Bool
prop_swap_right_local x = hidden_spaces x == hidden_spaces (swapDown  x)

-- rotation through the height of a stack gets us back to the start
prop_swap_all_l :: T -> Bool
prop_swap_all_l x = (foldr (const swapUp)  x [1..n]) == x
  where n = length (index x)

prop_swap_all_r :: T -> Bool
prop_swap_all_r x = (foldr (const swapDown) x [1..n]) == x
  where n = length (index x)

prop_swap_master_idempotent :: T -> Bool
prop_swap_master_idempotent x = swapMaster (swapMaster x) == swapMaster x
