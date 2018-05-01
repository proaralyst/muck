module Main where

import qualified Screen
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as C (stdout, stdin)
import Control.Concurrent (forkIO, MVar(..), newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Conduit ((.|), runConduit)
import System.Exit (exitSuccess)
import System.IO (BufferMode(..), hSetBuffering, hFlush, stdin, stdout)
import System.Posix.Signals (
    installHandler,
    Signal(..),
    Handler(..),
    keyboardSignal,
    keyboardTermination,
    softwareTermination)
import System.Process (CmdSpec(..))

tearDown :: MVar () -> IO ()
tearDown mvar = putMVar mvar ()

setup :: IO (MVar ())
setup = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    let killSigs =
            [ keyboardSignal
            , keyboardTermination
            , softwareTermination
            ]
    termVar <- newEmptyMVar
    let install sig = installHandler sig (Catch . tearDown $ termVar) Nothing
    install `mapM_` killSigs
    return termVar

main :: IO ()
main = do
    doExit <- setup
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    screen <- Screen.new (RawCommand "/usr/bin/bash" [])
    void . forkIO . runConduit $ Screen.outputConduit screen .| C.stdout
    void . forkIO . runConduit $ C.stdin .| Screen.inputConduit screen
    void . takeMVar $ doExit
    putStrLn ""
    putStrLn "Exiting"
    hFlush stdout
