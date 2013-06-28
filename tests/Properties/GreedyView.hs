{-# LANGUAGE ScopedTypeVariables #-}
module Properties.GreedyView where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.List (sortBy)

-- ---------------------------------------------------------------------
-- greedyViewing workspaces

-- greedyView sets the current workspace to 'n'
prop_greedyView_current :: T -> Gen Bool
prop_greedyView_current x  = do
    n <- arbitraryTag x
    return $ currentTag (greedyView n x) == n

-- greedyView leaves things unchanged for invalid workspaces
prop_greedyView_current_id :: T -> Gen Bool
prop_greedyView_current_id x = do
  n <- arbitrary `suchThat` \n' -> not $ n' `tagMember` x
  return $ currentTag (greedyView n x) == currentTag x

-- greedyView *only* sets the current workspace, and touches Xinerama.
-- no workspace contents will be changed.
prop_greedyView_local :: T -> Gen Bool
prop_greedyView_local x = do
    n <- arbitraryTag x
    return $ sortedWs x == sortedWs (greedyView n x)
  where
    sortedWs ss = sortBy (\s t -> tag s `compare` tag t) $
                                    workspace (current ss)
                                    : map workspace (visible ss) ++ hidden ss

-- greedyView is idempotent
prop_greedyView_idem :: T -> Gen Bool
prop_greedyView_idem x = do
  n <- arbitraryTag x
  return $ greedyView n (greedyView n x) == (greedyView n x)

-- greedyView is reversible, though shuffles the order of hidden/visible
prop_greedyView_reversible :: T -> Gen Bool
prop_greedyView_reversible x = do
    n <- arbitraryTag x
    return $ normal (greedyView n' (greedyView n x)) == normal x
    where n'  = currentTag x
