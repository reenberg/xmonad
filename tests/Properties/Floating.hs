{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Floating where

import Test.QuickCheck
import Instances

import XMonad.StackSet hiding (filter)

import qualified Data.Map as M

------------------------------------------------------------------------
-- properties for the floating layer:

prop_float_reversible :: NonEmptyWindowsStackSet -> Gen Bool
prop_float_reversible nex = do
  let NonEmptyWindowsStackSet x = nex
  w <- arbitraryWindow nex
  return $ sink w (float w geom x) == x
        where
            geom = RationalRect 100 100 100 100

prop_float_geometry :: NonEmptyWindowsStackSet -> Gen Bool
prop_float_geometry nex = do
    let NonEmptyWindowsStackSet x = nex
    w <- arbitraryWindow nex
    let s = float w geom x
    return $ M.lookup w (floating s) == Just geom
  where
    geom = RationalRect 100 100 100 100

prop_float_delete :: NonEmptyWindowsStackSet -> Gen Bool
prop_float_delete nex = do
    let NonEmptyWindowsStackSet x = nex
    w <- arbitraryWindow nex
    let s = float w geom x
        t = delete w s
    return $ not (w `member` t)
  where
    geom = RationalRect 100 100 100 100
