module Main (main) where

import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


main :: IO ()
main = defaultMain tests

t :: [Int] -> Bool
t n = last $ map (\s -> s==s) [1..599999]

tests :: [Test]
tests = [testProperty "Test 1" t
        , testProperty "Test 2" t
        , testGroup "text" [testProperty "Test 3.1" t,
                            testProperty "Test 3.2" t]]
