module Test.TermEmu
    ( root
    )
    where

import qualified TermEmu as T
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A

import Data.Word (Word8(..))
import Test.Tasty
import Test.Tasty.HUnit

testParser :: (Show a, Eq a) => A.Parser a -> [Word8] -> a -> Assertion
testParser parser input expected =
    assert (A.parse parser (BS.pack input)) (\ partial ->
        assert (partial BS.empty) (const $ assertFailure "Partial parse"))
  where
    assert res f = case res of
        A.Fail _rest _context error -> assertFailure $ "Parse failed " ++ error
        A.Partial partial -> f partial
        A.Done _ res -> res @?= expected

number :: TestTree
number = testGroup "number"
    [ tc "single digit" $ test [0x30] 0
    , tc "a few digits" $ test [0x30, 0x31, 0x38] 18
    ]
  where
    tc = testCase
    test = testParser T.number

params :: TestTree
params = testGroup "params"
    [ tc "Handles empty string" $ test [] []
    , tc "Single param" $ test [0x30] [0]
    ]
  where
    tc = testCase
    test = testParser T.params

root :: TestTree 
root = testGroup "TermEmu"
    [ params, number
    ]

