
import Test.Tasty
import Test.Tasty.HUnit

testCase1 :: Assertion
testCase1 = do
    result <- return 1
    1 @?= result

testTree :: TestTree
testTree = testGroup "test tree"
    [ testCase
        "test case 1"
        testCase1
    ]

tests :: TestTree
tests = testGroup "All tests" [testTree]

main :: IO ()
main = defaultMain tests
