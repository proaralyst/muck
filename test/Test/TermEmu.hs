module Test.TermEmu
    ( root
    )
    where

import qualified TermEmu as T
import qualified TermEmu.SGR as SGR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec.ByteString as A

import Data.List (intercalate)
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

testParser :: (Show a, Eq a) => A.Parser a -> BS.ByteString -> a -> Assertion
testParser parser input expected =
    assert (A.parse parser input) (\ partial ->
        assert (partial BS.empty) (const $ assertFailure "Partial parse"))
  where
    assert res f = case res of
        A.Fail rest contexts error ->
            assertFailure $ printf "Parse failed: %s; remaining = '%s' (Contexts: '%s')"
                error
                (BC.unpack rest)
                (intercalate "', '" contexts)
        A.Partial partial -> f partial
        A.Done _ res -> res @?= expected

number :: TestTree
number = testGroup "number"
    [ tc "single digit" $ test [0x30] 0
    , tc "a few digits" $ test [0x30, 0x31, 0x38] 18
    ]
  where
    tc = testCase
    test input = testParser T.number (BS.pack input)

params :: TestTree
params = testGroup "params"
    [ tc "Handles empty string" $ test [] []
    , tc "Single param" $ test [0x30] [0]
    ]
  where
    tc = testCase
    test input = testParser T.params (BS.pack input)


full :: TestTree
full = testGroup "full"
    [ tc "plain text" $ test "f" (T.Raw 'f')
    , tc "SGR 256 colour" $ test "\ESC[38;5;34m"
        (T.CSI $ T.SGR [Right . SGR.Fg $ SGR.Colour256 34])
    , tc "SGR multi" $ test "\ESC[0;1;5;4m" . T.CSI . T.SGR $
        Right <$> [ SGR.Clear, SGR.Bright, SGR.Blink, SGR.Underscore]
    ]
  where
    tc = testCase
    test input = testParser T.parse (BC.pack input)


root :: TestTree 
root = testGroup "TermEmu"
    [ number, params, full
    ]

