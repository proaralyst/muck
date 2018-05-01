module Main where

import qualified Screen
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as C
import Control.Concurrent (forkIO, MVar(..), newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.ByteString (ByteString(..), hPut)
import Data.Conduit (ConduitT(..), (.|), runConduit, bracketP, awaitForever, yieldM)
import System.Exit (exitSuccess)
import System.IO
    ( BufferMode(..)
    , IOMode(..)
    , hSetBuffering
    , hFlush
    , hSetEcho
    , stdin
    , stdout
    , hClose
    , openFile
    )
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
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    let killSigs =
            [ keyboardSignal
            , keyboardTermination
            , softwareTermination
            ]
    termVar <- newEmptyMVar
    let install sig = installHandler sig (Catch $ putMVar termVar ()) Nothing
    install `mapM_` killSigs
    hSetEcho stdin False
    return termVar

handleOutput :: Screen.Screen -> IO ()
handleOutput screen = do
    log <- openFile "/tmp/log" WriteMode
    hSetBuffering log NoBuffering
    runConduit $ Screen.outputConduit screen
        .| logger log
        .| C.stdout
    hClose log
  where
    logger handle = awaitForever $ \ str -> yieldM $ do
        hPut handle str
        return str

main :: IO ()
main = do
    doExit <- setup
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    screen <- Screen.new (RawCommand "/usr/bin/bash" [])
    void . forkIO . runConduit $ C.stdin .| Screen.inputConduit screen
    void . forkIO . handleOutput $ screen
    void . takeMVar $ doExit
    putStrLn ""
    putStrLn "Exiting"
    hSetEcho stdin True
    hFlush stdout
    exitSuccess
