module Main where

import qualified Screen
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as C (stdout, stdin)
import Control.Concurrent (forkIO)
import Data.Conduit ((.|), runConduit)
import System.IO (BufferMode(..), hSetBuffering, stdin, stdout)
import System.Process (CmdSpec(..))

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    screen <- Screen.new (RawCommand "/usr/bin/bash" [])
    _ <- forkIO . runConduit $ Screen.outputConduit screen .| C.stdout
    runConduit $ C.stdin .| Screen.inputConduit screen
