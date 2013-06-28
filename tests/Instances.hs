{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- We have orphaned instances by design!
module Instances where

import Test.QuickCheck

import Utils

import XMonad.StackSet
import Control.Monad

import Data.List (nub)

--import Debug.Trace

--
-- The all important Arbitrary instance for StackSet.
--
instance (Integral i, Integral s, Eq a, Arbitrary a, Arbitrary l, Arbitrary sd)
         => Arbitrary (StackSet i l a s sd) where
  arbitrary = do
      -- TODO: Fix this to be a reasonable higher number, Possibly use PositiveSized
      numWs        <- choose (1, 20)    -- number of workspaces, there must be at least 1.
      numScreens   <- choose (1, numWs) -- number of physical screens, there must be at least 1
      lay <- arbitrary                  -- pick any layout

      wsIdxInFocus <- choose (1, numWs) -- pick index of WS to be in focus

      -- The same screen id's will be present in the list, with high possibility.
      scrs  <- replicateM numScreens arbitrary

      -- Generate a list of "windows" for each workspace.
      wsWindows  <- vector numWs :: Gen [[a]]

      -- Pick a random window "number" in each workspace, to give focus.
      wsFocus <- sequence [ if null windows
                          then return Nothing
                          else liftM Just $ choose (0, length windows - 1)
                        | windows <- wsWindows ]

      let wsTags = [1 .. fromIntegral numWs]
          wsFocusWindows = zip wsFocus wsWindows
          -- Windows to populate into the StackSet at workspace=tag and with a
          -- focused window for each ws [tag, [(focus, windows)]]
          wsTFW = zip wsTags wsFocusWindows
          -- Initial StackSet to be populated
          initSs = new lay wsTags scrs
      return $
        view (fromIntegral wsIdxInFocus) $
        foldr (\(tg, (fcs, wds)) ss -> -- Fold through all generated (tags, [(focus, windows)]).
                -- set workspace active by tag and fold through all
                -- windows while inserting them.  Apply the given number
                -- of `focusUp` on the resulting StackSet.
                applyN fcs focusUp $ foldr insertUp (view tg ss) wds
              ) initSs wsTFW


--
-- Just generate StackSets with Char elements.
--
type Tag = Int
type Layout = Int
type Window = Char
type SrcId = Int
type SrcDetail = Int
type T = StackSet Tag Layout Window SrcId SrcDetail



newtype EmptyStackSet = EmptyStackSet T
    deriving Show

instance Arbitrary EmptyStackSet where
    arbitrary = do
        (NonEmptyNubList ns)  <- arbitrary
        (NonEmptyNubList sds) <- arbitrary
        l <- arbitrary
        -- there cannot be more screens than workspaces:
        return . EmptyStackSet . new l ns $ take (min (length ns) (length sds)) sds



newtype NonEmptyWindowsStackSet = NonEmptyWindowsStackSet T
    deriving Show

instance Arbitrary NonEmptyWindowsStackSet where
  arbitrary =
    NonEmptyWindowsStackSet `fmap` (arbitrary `suchThat` (not . null . allWindows))



newtype SizedPositive = SizedPositive Int
    deriving (Eq, Ord, Show, Read)

instance Arbitrary SizedPositive where
  arbitrary = sized $ \s -> do x <- choose (1, max 1 s)
                               return $ SizedPositive x



newtype NonEmptyNubList a = NonEmptyNubList [a]
    deriving ( Eq, Ord, Show, Read )

instance (Eq a, Arbitrary a) => Arbitrary (NonEmptyNubList a) where
  arbitrary   = NonEmptyNubList `fmap` ((liftM nub arbitrary) `suchThat` (not . null))



-- | Pull out an arbitrary tag from the StackSet. This removes the need for the
-- precondition "n `tagMember x` in many properties and thus reduces the number
-- of discarded tests.
--
--  n <- arbitraryTag x
--
-- We can do the reverse with a simple `suchThat`:
--
-- n <- arbitrary `suchThat` \n' -> not $ n' `tagMember` x
arbitraryTag :: T -> Gen Tag
arbitraryTag x = do
  let ts = tags x
  -- There must be at least 1 workspace, thus at least 1 tag.
  idx <- choose (0, (length ts) - 1)
  return $ ts!!idx

-- | Pull out an arbitrary window from a StackSet that is guaranteed to have a
-- non empty set of windows. This eliminates the precondition "i `member` x" in
-- a few properties.
--
--
-- foo (nex :: NonEmptyWindowsStackSet) = do
--   let NonEmptyWindowsStackSet x = nex
--   w <- arbitraryWindow nex
--   return $ .......
--
-- We can do the reverse with a simple `suchThat`:
--
--   n <- arbitrary `suchThat` \n' -> not $ n `member` x
--
arbitraryWindow :: NonEmptyWindowsStackSet -> Gen Window
arbitraryWindow (NonEmptyWindowsStackSet x) = do
  let ws = allWindows x
  -- We know that there are at least 1 window in a NonEmptyWindowsStackSet.
  idx <- choose(0, (length ws) - 1)
  return $ ws!!idx
