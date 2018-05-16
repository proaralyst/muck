module Main where

import qualified Data.Attoparsec as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Pipes.Prelude as P
import qualified Screen
import qualified System.IO as IO
import qualified TermEmu.Parser as TP

import Control.Concurrent (forkIO, MVar(..), newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void, forever)
import Control.Monad.State (runStateT)
import Data.ByteString (ByteString(..), hPut)
import Data.List (intercalate)
import Pipes
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
import Text.Printf (printf)

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

logParse :: Consumer ByteString IO ()
logParse = do
    handle <- lift $ do
        handle <- IO.openFile "/tmp/log.parsed" IO.WriteMode
        IO.hSetBuffering handle IO.NoBuffering
        return handle
    await >>= go handle . parse
  where
    parse = A.parse TP.parse

    go handle (A.Fail remaining contexts msg) = do
        lift . IO.hPutStrLn handle
            $ printf "Parse error: %s (contexts: '%s'). Input: {%s}"
                msg (intercalate "', '" contexts)
                (show $ fromEnum <$> BC.unpack remaining)
        chunk <- await
        go handle (parse chunk)

    go handle (A.Partial continuation) = do
        chunk <- await
        go handle (continuation chunk)

    go handle (A.Done remaining token) = do
        lift $ IO.hPrint handle token
        if remaining == mempty
        then await >>= go handle . parse
        else go handle (parse remaining)

handleOutput :: Screen.Screen -> IO ()
handleOutput screen = do
    log <- IO.openFile "/tmp/log" IO.WriteMode
    IO.hSetBuffering log IO.NoBuffering
    runEffect $ Screen.output screen
        >-> (P.tee . P.mapM_ $ hPut log)
        >-> P.tee logParse
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
