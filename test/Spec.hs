{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Either

import qualified GithubWebhook.RequestParser as RP

testCommentParsingSuccess :: Assertion
testCommentParsingSuccess = RP.parse "hueue merge a b" @?= Right (RP.ParsedRequest "merge" "a" "b")

testCommentParsingFailure1 :: Assertion
testCommentParsingFailure1 = assertBool "Should fail" (isLeft $ RP.parse "nothueue merge a b")

testCommentParsingFailure2 :: Assertion
testCommentParsingFailure2 = assertBool "Should fail" (isLeft $ RP.parse "hueue merge a")

-- currently fails
testCommentParsingFailure3 :: Assertion
testCommentParsingFailure3 = assertBool "Should fail" (isLeft $ RP.parse "hueue merge a b c")

testCommentParsingFailure4 :: Assertion
testCommentParsingFailure4 = assertBool "Should fail" (isLeft $ RP.parse "hueue merge")

testCommentParsingFailure5 :: Assertion
testCommentParsingFailure5 = assertBool "Should fail" (isLeft $ RP.parse "hueue")

-- TODO: replaceFirst
-- TODO: trimming

testParsing :: TestTree
testParsing = testGroup "Parsing tests"
    [ testCase
        "Test comment parsing success case"
        testCommentParsingSuccess
    , testCase
        "Test comment parsing failure case 1"
        testCommentParsingFailure1
    , testCase
        "Test comment parsing failure case 2"
        testCommentParsingFailure2
    --, testCase
    --    "Test comment parsing failure case 3"
    --    testCommentParsingFailure3
    , testCase
        "Test comment parsing failure case 4"
        testCommentParsingFailure4
    , testCase
        "Test comment parsing failure case 5"
        testCommentParsingFailure5
    ]

githubWebhookTestTree :: TestTree
githubWebhookTestTree = testGroup "GithubWebhook tests" [testParsing]

tests :: TestTree
tests = testGroup "All tests" [githubWebhookTestTree]

main :: IO ()
main = defaultMain tests
