module TermEmu
    ( ParserState(..)
    ) where

import Prelude hiding (print, read)

import qualified Data.Word as Word
import qualified TermEmu.Automata as A
import Control.Lens
import Control.Monad.Free (Free(..), liftF)
import Safe (readMay)

data ParserState =
    ParserState
    { _automataState :: A.State
    , _params        :: [Word.Word8]
    , _intermediate  :: [Word.Word8]
    , _final         :: [Word.Word8]
    }

makeLenses ''ParserState

data DisplayErase =
      FromCursor
    | ToCursor
    | WholeDisplay

eraseAmount :: Word.Word8 -> DisplayErase
eraseAmount 0 = FromCursor
eraseAmount 1 = ToCursor
eraseAmount 2 = WholeDisplay
eraseAmount _ = FromCursor

type Param = Maybe Natural

data CSI = 
    | ICH Param
    | CUU Param
    | CUD Param
    | CUF Param
    | CUB Param
    | CNL Param
    | CPL Param
    | CHA Param
    | CUP Param Param
    | ED  Param
    | EL  Param
    | IL  Param
    | DL  Param
    | DCH Param
    | SU  Param
    | ECH Param
    | CBT
    | REP Param 
    | DA  Param
    | VPA Param
    | HVP Param Param
    | TBC Param
    | SM SM
    | RM RM
    | SGR SGR
    | DSR DSR
    | DECSTBM Param Param 
    | SCP Param Param

data Out =
      Raw Word.Word8
    | Esc
    | CSI CSI
    | Debug String

uintParam :: State ParserState Natural
uintParam state =
    use params >>= extract
  where
    extract [] = return 0
    extract (p:ps) = do
        params .= ps
        return p

-- TODO: clear state
-- TODO: change default behaviour to Nothing so the non-zero defaults work
-- TODO: find out what WINOPS & RCP are
dispatchCsi :: Word.Word8 -> State ParserState Out
dispatchCsi '@' = Out . ICH <$> uintParam
dispatchCsi 'A' = Out . CUU <$> uintParam
dispatchCsi 'B' = Out . CUD <$> uintParam
dispatchCsi 'C' = Out . CUF <$> uintParam
dispatchCsi 'D' = Out . CUB <$> uintParam
dispatchCsi 'E' = Out . CNL <$> uintParam
dispatchCsi 'F' = Out . CPL <$> uintParam
dispatchCsi 'G' = Out . CHA <$> uintParam
dispatchCsi 'H' = Out . CUP <$> uintParam <*> uintparam
dispatchCsi 'J' = Out . ED . eraseAmount <$> uintParam
dispatchCsi 'K' = Out . EL . eraseAmount <$> uintParam
dispatchCsi 'L' = Out . IL <$> uintParam
dispatchCsi 'M' = Out . DL <$> uintParam
dispatchCsi 'P' = Out . DCH <$> uintParam
dispatchCsi 'S' = Out . SU <$> uintParam
dispatchCsi 'X' = Out . ECH <$> uintParam
dispatchCsi 'Z' = Out . CBT
dispatchCsi 'b' = Out . REP <$> uintParam -- this one might need some more state
dispatchCsi 'c' = dispatchDA -- private: >
dispatchCsi 'd' = Out . VPA <$> uintParam
dispatchCsi 'f' = Out . HVP <$> uintParam <*> uintparam -- Same as CUP
-- TODO: TabClear only allows 0 or 3 (at cursor or all)
dispatchCsi 'g' = Out . TBC <$> uintParam
dispatchCsi 'h' = dispatchSetMode -- private: ?
dispatchCsi 'l' = dispatchResetMode -- private: ?
dispatchCsi 'm' = dispatchSelectGraphics
dispatchCsi 'n' = dispatchDSR -- private: ' '
dispatchCsi 'r' = Out . DECSTBM <$> uintParam <*> uintParam
dispatchCsi 's' = Out . SCP <$> uintParam <*> uintParam
dispatchCsi x   = do
    params <- use params
    intermediate <- use intermediate
    return . Debug $ printf
        "Unrecognised CSI: %c; params = {%s}; intermediate = {%s}"
        params intermediate

init :: ParserState
init = ParserState A.Ground [] [] []

applyAction :: Word.Word8 -> A.Action -> Program ()
applyAction _ A.Clear = do
    modify $ set params mempty
    modify $ set intermediate mempty
    modify $ set final mempty
applyAction x A.Execute =
    -- TODO: actually execute
    applyAction x A.Clear
-- TODO: actually handle device control strings?
applyAction x A.Hook = applyAction x A.Clear
applyAction _ A.Unhook = return ()
applyAction _ A.Put = return ()
applyAction x A.OscStart = applyAction x A.Clear
applyAction x A.OscEnd = applyAction x A.Clear
applyAction x A.EscapeDispatch = applyAction x A.Clear
applyAction x A.CsiDispatch = applyAction x A.Clear
applyAction x A.Print = print x
applyAction x A.Collect = modify $ over intermediate (x:)
applyAction _ A.OscPut = return ()
applyAction x A.Param = modify $ over params (x:)
applyAction _ A.Nop = return ()

transitionAction :: Word.Word8 -> A.State -> A.State -> (A.State -> A.Action) -> State (Maybe Word.Word8)
transitionAction char oldState newState f
    | oldState == newState = applyAction char $ f newState
    | otherwise = return ()

process :: Word.Word8 -> State ParserState (Maybe Word.Word8)
process char = do
    oldState <- use automataState
    let (nextState, action) = A.nextState oldState char
    let doAction = transitionAction char oldState nextState
    doAction A.entryAction
    applyAction char action
    doAction A.exitAction
    modify $ set automataState nextState
