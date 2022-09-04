{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Test.HUnit
import Hint (makeHint)
import Code
import Game
import Data.Maybe (fromJust)

testHint_Non_Duplicate_Digits :: Test
testHint_Non_Duplicate_Digits = 
    TestCase $ assertEqual msg ans $ mkHint code (Guess guess)
    where
        code  = mkCode 6 2 4 3
        guess = mkCode 1 2 3 4
        ans   = "1,0,0" -- 2, 3 and 4 are included, 2 also has the correct position
        msg   = "Test Non Duplicate Digits"

-- When there are duplicate digits in the guess, they cannot all be awarded a key bit
-- unless they correspond to the same number of duplicate digits in the hidden code.

testHint_Duplicate_Digits_No1 :: Test
testHint_Duplicate_Digits_No1 =
    TestCase $ assertEqual msg ans $ mkHint code (Guess guess)
    where
        code  = mkCode 6 2 4 3
        guess = mkCode 6 2 2 5
        ans   = "1,1"
        msg   = "Test Duplicate Digits #1"

testHint_Duplicate_Digits_No2 :: Test
testHint_Duplicate_Digits_No2 =
    TestCase $ assertEqual msg ans $ mkHint code (Guess guess)
    where
        code  = mkCode 5 2 5 6
        guess = mkCode 2 2 4 4
        ans   = "1"
        msg   = "Test Duplicate Digits #2"

testHint_Duplicate_Digits_No3 :: Test
testHint_Duplicate_Digits_No3 =
    TestCase $ assertEqual msg ans $ mkHint code (Guess guess)
    where
        code  = mkCode 6 4 4 3
        guess = mkCode 4 1 2 4
        ans   = "0,0"
        msg   = "Test Duplicate Digits #3"

testHint_Duplicate_Digits_No4 :: Test
testHint_Duplicate_Digits_No4 =
    TestCase $ assertEqual msg ans $ mkHint code (Guess guess)
    where
        code  = mkCode 6 4 2 3
        guess = mkCode 2 2 5 2
        ans   = "0"
        msg   = "Test Duplicate Digits #4"

testHint_Duplicate_Digits_No5 :: Test
testHint_Duplicate_Digits_No5 =
    TestCase $ assertEqual msg ans $ mkHint code (Guess guess)
    where
        code  = mkCode 6 1 6 3
        guess = mkCode 1 1 3 6
        ans   = "1,0,0"
        msg   = "Test Duplicate Digits #5"

testMakeCode_Correct_No1 :: Test
testMakeCode_Correct_No1 =
    TestCase $ assertEqual msg ans $ Code.fromString "1234"
    where
        ans = Just $ mkCode 1 2 3 4
        msg = "Test correct code #1"

testMakeCode_Correct_No2 :: Test
testMakeCode_Correct_No2 =
    TestCase $ assertEqual msg ans $ Code.fromString "6543"
    where
        ans = Just $ mkCode 6 5 4 3
        msg = "Test correct code #2"

testMakeCode_Too_Short :: Test
testMakeCode_Too_Short =
    TestCase $ assertEqual msg ans $ Code.fromString "123"
    where
        ans = Nothing
        msg = "Test incorrect code: too short"

testMakeCode_Too_Long :: Test
testMakeCode_Too_Long =
    TestCase $ assertEqual msg ans $ Code.fromString "12345"
    where
        ans = Nothing
        msg = "Test incorrect code: too long"

testMakeCode_Non_Digits :: Test
testMakeCode_Non_Digits =
    TestCase $ assertEqual msg ans $ Code.fromString "12e3"
    where
        ans = Nothing
        msg = "Test incorrect code: non digits"

testMakeCode_Invalid_Digits :: Test
testMakeCode_Invalid_Digits =
    TestCase $ assertEqual msg ans $ Code.fromString "0237"
    where
        ans = Nothing
        msg = "Test incorrect code: invalid digits"

mkCode :: Int -> Int -> Int -> Int -> Code
mkCode a b c d = fromJust $ Code.fromList [a, b, c, d]

mkHint :: Code -> Guess -> String
mkHint c g = show $ makeHint c g

main :: IO Counts
main = runTestTT $ TestList
    [ testHint_Non_Duplicate_Digits
    , testHint_Duplicate_Digits_No1
    , testHint_Duplicate_Digits_No2
    , testHint_Duplicate_Digits_No3
    , testHint_Duplicate_Digits_No4
    , testMakeCode_Correct_No1
    , testMakeCode_Correct_No2
    , testMakeCode_Too_Short
    , testMakeCode_Too_Long
    , testMakeCode_Non_Digits
    , testMakeCode_Invalid_Digits
    ]
