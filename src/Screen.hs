module Screen
    ( Screen(..)
    , pty
    , child
    , new
    , outputConduit
    , inputConduit
    ) where

import qualified Data.Word as Word
import Control.Lens
import Data.ByteString (ByteString(..), hGetSome)
import Data.Conduit (ConduitT)
import Data.Conduit.Combinators (sourceHandle, sinkHandle)
import System.IO (Handle(..), hSetBuffering, BufferMode(..))
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.IO (fdToHandle)
import System.Process
    (CmdSpec, ProcessHandle(..), CreateProcess(..), createProcess, StdStream(..))

data Screen = Screen
    { _pty :: Handle
    , _child :: ProcessHandle
    }

makeLenses ''Screen


new :: CmdSpec -> IO Screen
new cmdspec = do
    let toHandle fd = do
            handle <- fdToHandle fd
            hSetBuffering handle NoBuffering
            return handle
    (masterFd, slaveFd) <- openPseudoTerminal
    master <- toHandle masterFd
    slave <- toHandle slaveFd
    (_, _, _, ph) <- createProcess CreateProcess
        { cmdspec = cmdspec
        , cwd = Nothing
        , env = Nothing
        , std_in = UseHandle slave
        , std_out = UseHandle slave
        , std_err = UseHandle slave
        , close_fds = True
        , create_group = True
        , delegate_ctlc = False
        , detach_console = True
        , create_new_console = True
        , new_session = True
        , child_group = Nothing
        , child_user = Nothing
        , use_process_jobs = False
        }
    return $ Screen master ph

outputConduit :: Screen -> ConduitT i ByteString IO ()
outputConduit screen = sourceHandle $ screen^.pty

inputConduit :: Screen -> ConduitT ByteString o IO ()
inputConduit screen = sinkHandle $ screen^.pty
