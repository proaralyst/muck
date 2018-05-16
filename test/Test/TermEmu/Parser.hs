module Test.TermEmu.Parser
    ( root
    )
    where

import qualified TermEmu.Parser as P
import qualified TermEmu.Parser.SGR as SGR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec.ByteString as A

import Data.List (intercalate)
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
    test input = testParser P.number (BS.pack input)

params :: TestTree
params = testGroup "params"
    [ tc "Handles empty string" $ test [] []
    , tc "Single param" $ test [0x30] [Just 0]
    , tc "Multiple with defaults" $ test
        [0x30, 0x3b, 0x37, 0x3b, 0x3b] [Just 0, Just 7, Nothing, Nothing]
    ]
  where
    tc = testCase
    test input = testParser P.params (BS.pack input)

control :: TestTree
control = testGroup "c0"
    [ tc "Bell" $ test [0x07] P.Bell
    , tc "CR" $ test [0x0D] P.CarriageReturn
    ]
  where
    tc = testCase
    test input = testParser P.control (BS.pack input)



full :: TestTree
full = testGroup "full"
    [ tc "plain text" $ test "foo" (P.Raw "foo")
    , tc "SGR 256 colour" $ test "\ESC[38;5;34m"
        (P.CSI $ P.SGR [Right . SGR.Fg $ SGR.Colour256 34])
    , tc "SGR multi" $ test "\ESC[0;1;5;4m" . P.CSI . P.SGR $
        Right <$> [ SGR.Clear, SGR.Bright, SGR.Blink, SGR.Underscore]
    , tc "No params SGR" $ test "\ESC[m" . P.CSI $ P.SGR []
    ]
  where
    tc = testCase
    test input = testParser P.parse (BC.pack input)


root :: TestTree
root = testGroup "TermEmu"
    [ number, params, control, full
    ]

