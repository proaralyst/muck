module Screen
    ( Screen(..)
    , pty
    , child
    , new
    , output
    , input
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import Pipes
import Pipes.ByteString (fromHandle, toHandle)
import System.IO (Handle, hSetBuffering, BufferMode(..))
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.IO (fdToHandle)
import System.Process
    (CmdSpec, ProcessHandle, CreateProcess(..), createProcess, StdStream(..))

data Screen = Screen
    { _pty   :: !Handle
    , _child :: !ProcessHandle
    }

makeLenses ''Screen


new :: CmdSpec -> IO Screen
new spec = do
    let toHandle fd = do
            handle <- fdToHandle fd
            hSetBuffering handle NoBuffering
            return handle
    (masterFd, slaveFd) <- openPseudoTerminal
    master <- toHandle masterFd
    slave <- toHandle slaveFd
    (_, _, _, ph) <- createProcess CreateProcess
        { cmdspec = spec
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

output :: Screen -> Producer ByteString IO ()
output screen = fromHandle $ screen^.pty

input :: Screen -> Consumer ByteString IO ()
input screen = toHandle $ screen^.pty
