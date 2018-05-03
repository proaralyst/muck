module Main where

import qualified Screen
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.ByteString as BS
import qualified System.IO as IO
import Control.Concurrent (forkIO, MVar(..), newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.ByteString (ByteString(..), hPut)
import Pipes.ByteString (stdin, stdout)
import System.Exit (exitSuccess)
import System.Posix.Signals
    ( installHandler
    , Signal(..)
    , Handler(..)
    , keyboardSignal
    , keyboardTermination
    , softwareTermination
    )
import System.Process (CmdSpec(..))

setup :: IO (MVar ())
setup = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetBuffering IO.stdout IO.NoBuffering
    let killSigs =
            [ keyboardSignal
            , keyboardTermination
            , softwareTermination
            ]
    termVar <- newEmptyMVar
    let install sig = installHandler sig (Catch $ putMVar termVar ()) Nothing
    install `mapM_` killSigs
    IO.hSetEcho IO.stdin False
    return termVar

handleOutput :: Screen.Screen -> IO ()
handleOutput screen = do
    log <- IO.openFile "/tmp/log" IO.WriteMode
    IO.hSetBuffering log IO.NoBuffering
    runEffect $ Screen.output screen
        >-> (P.tee . P.mapM_ $ hPut log)
        >-> stdout
    IO.hClose log

main :: IO ()
main = do
    doExit <- setup
    screen <- Screen.new (RawCommand "/usr/bin/bash" [])
    void . forkIO . runEffect $ stdin >-> Screen.input screen
    void . forkIO $ handleOutput screen
    void . takeMVar $ doExit
    putStrLn ""
    putStrLn "Exiting"
    IO.hSetEcho IO.stdin True
    IO.hFlush IO.stdout
    exitSuccess
