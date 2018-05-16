import Test.Tasty
import qualified Test.TermEmu.Parser as TE (root)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ TE.root ]
