import Test.HUnit
import Lib
import Code
import Game

testHint_Non_Duplicate_Digits :: Test
testHint_Non_Duplicate_Digits = 
    TestCase $ assertEqual msg ans $ hint code (Guess guess)
    where
        code  = Code 6 2 4 3
        guess = Code 1 2 3 4
        ans   = "1,0,0" -- 2, 3 and 4 are included, 2 also has the correct position
        msg   = "Test Non Duplicate Digits"

-- When there are duplicate digits in the guess, they cannot all be awarded a key bit
-- unless they correspond to the same number of duplicate digits in the hidden code.

testHint_Duplicate_Digits_No1 :: Test
testHint_Duplicate_Digits_No1 =
    TestCase $ assertEqual msg ans $ hint code (Guess guess)
    where
        code  = Code 6 2 4 3
        guess = Code 6 2 2 5
        ans   = "1,1"
        msg   = "Test Duplicate Digits #1"

testHint_Duplicate_Digits_No2 :: Test
testHint_Duplicate_Digits_No2 =
    TestCase $ assertEqual msg ans $ hint code (Guess guess)
    where
        code  = Code 5 2 5 6
        guess = Code 2 2 4 4
        ans   = "1"
        msg   = "Test Duplicate Digits #2"

testHint_Duplicate_Digits_No3 :: Test
testHint_Duplicate_Digits_No3 =
    TestCase $ assertEqual msg ans $ hint code (Guess guess)
    where
        code  = Code 6 4 4 3
        guess = Code 4 1 2 4
        ans   = "0,0"
        msg   = "Test Duplicate Digits #3"

testHint_Duplicate_Digits_No4 :: Test
testHint_Duplicate_Digits_No4 =
    TestCase $ assertEqual msg ans $ hint code (Guess guess)
    where
        code  = Code 6 4 2 3
        guess = Code 2 2 5 2
        ans   = "0"
        msg   = "Test Duplicate Digits #4"

testMakeCode_Correct_No1 :: Test
testMakeCode_Correct_No1 =
    TestCase $ assertEqual msg ans $ makeCode "1234"
    where
        ans = Just $ Code 1 2 3 4
        msg = "Test correct code #1"

testMakeCode_Correct_No2 :: Test
testMakeCode_Correct_No2 =
    TestCase $ assertEqual msg ans $ makeCode "6543"
    where
        ans = Just $ Code 6 5 4 3
        msg = "Test correct code #2"

testMakeCode_Too_Short :: Test
testMakeCode_Too_Short =
    TestCase $ assertEqual msg ans $ makeCode "123"
    where
        ans = Nothing
        msg = "Test incorrect code: too short"

testMakeCode_Too_Long :: Test
testMakeCode_Too_Long =
    TestCase $ assertEqual msg ans $ makeCode "12345"
    where
        ans = Nothing
        msg = "Test incorrect code: too long"

testMakeCode_Non_Digits :: Test
testMakeCode_Non_Digits =
    TestCase $ assertEqual msg ans $ makeCode "12e3"
    where
        ans = Nothing
        msg = "Test incorrect code: non digits"

testMakeCode_Invalid_Digits :: Test
testMakeCode_Invalid_Digits =
    TestCase $ assertEqual msg ans $ makeCode "0237"
    where
        ans = Nothing
        msg = "Test incorrect code: invalid digits"

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
