module Properties.Failure where

import XMonad.StackSet hiding (filter)

import qualified Control.Exception.Extensible as C
import System.IO.Unsafe

-- ---------------------------------------------------------------------
-- testing for failure

-- and help out hpc
prop_abort :: a -> Bool
prop_abort _ = unsafePerformIO $ C.catch (abort "fail")
                                         (\(C.SomeException e) -> return $  show e == "xmonad: StackSet: fail" )


{-

-- new should fail with an abort, when given an empty list of screens
prop_new_screens_abort :: [i] -> Bool
prop_new_screens_abort ws = unsafePerformIO $ C.catch f
                                         (\(C.SomeException e) -> return $ show e == "xmonad: StackSet: non-positive argument to StackSet.new" )
   where
     f = new undefined{-layout-} ws [] `seq` return False


-- new should fail with an abort, when given an empty list of screens
prop_new_workspace_abort :: [screens] -> Bool
prop_new_workspace_abort ss = unsafePerformIO $ C.catch f
                                         (\(C.SomeException e) -> return $ show e == "xmonad: StackSet: non-positive argument to StackSet.new" )
   where
     f = new undefined{-layout-} [] ss `seq` return False

-}

-- TODO: Test for fewer workspaces than screens. That should also fail with an abort.


-- TODO: Fix this?
-- prop_view_should_fail = view {- with some bogus data -}
