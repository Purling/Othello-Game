module Dragons.TestMain where

import AITests
import CheckersTests
import Testing

allTests :: Test
allTests = TestGroup "All Tests"
  [ checkersTests
  , aiTests
  ]

testMain :: IO ()
testMain = runTests allTests
