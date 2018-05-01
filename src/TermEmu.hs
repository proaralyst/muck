module TermEmu
    ( ParserState(..)
    ) where

import Prelude hiding (print, read)

import qualified Data.Text as T
import qualified Data.Word as Word
import qualified TermEmu.Automata as A
import qualified TermEmu.SGR as SGR
import Control.Lens
import Control.Monad.State (State, put, get, gets)
import Numeric.Natural (Natural)
import Safe (readMay)
import Text.Printf (printf)

data ParserState =
    ParserState
    { _automataState :: A.State
    , _params        :: [Word.Word8]
    , _intermediate  :: [Word.Word8]
    , _final         :: [Word.Word8]
    }

makeLenses ''ParserState

-- TODO: DA SM RM & DSR

type Param = Maybe Natural

data CSI =
      ICH Param
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
    | DA
    | VPA Param
    | HVP Param Param
    | TBC Param
    | SM
    | RM
    | SGR [Either T.Text SGR.SGR]
    | DSR
    | DECSTBM Param Param
    | SCP Param Param

data Out =
      Raw Word.Word8
    | Esc
    | CSI CSI
    | Debug T.Text

uintParam :: State ParserState Param
uintParam =
    use params >>= extract
  where
    extract :: [Word.Word8] -> State ParserState Param
    extract [] = return Nothing
    extract (p:ps) = do
        params .= ps
        return . Just . fromIntegral $ p

-- TODO: clear state
-- TODO: change default behaviour to Nothing so the non-zero defaults work
-- TODO: find out what WINOPS & RCP are
dispatchCsi :: Char -> State ParserState Out
dispatchCsi '@' = CSI . ICH <$> uintParam
dispatchCsi 'A' = CSI . CUU <$> uintParam
dispatchCsi 'B' = CSI . CUD <$> uintParam
dispatchCsi 'C' = CSI . CUF <$> uintParam
dispatchCsi 'D' = CSI . CUB <$> uintParam
dispatchCsi 'E' = CSI . CNL <$> uintParam
dispatchCsi 'F' = CSI . CPL <$> uintParam
dispatchCsi 'G' = CSI . CHA <$> uintParam
dispatchCsi 'H' = CSI <$> (CUP <$> uintParam <*> uintParam)
dispatchCsi 'J' = CSI . ED <$> uintParam
dispatchCsi 'K' = CSI . EL <$> uintParam
dispatchCsi 'L' = CSI . IL <$> uintParam
dispatchCsi 'M' = CSI . DL <$> uintParam
dispatchCsi 'P' = CSI . DCH <$> uintParam
dispatchCsi 'S' = CSI . SU <$> uintParam
dispatchCsi 'X' = CSI . ECH <$> uintParam
dispatchCsi 'Z' = return $ CSI CBT
dispatchCsi 'b' = CSI . REP <$> uintParam -- this one might need some more state
dispatchCsi 'c' = return $ CSI DA -- private: >
dispatchCsi 'd' = CSI . VPA <$> uintParam
dispatchCsi 'f' = CSI <$> (HVP <$> uintParam <*> uintParam) -- Same as CUP
-- TODO: TabClear only allows 0 or 3 (at cursor or all)
dispatchCsi 'g' = CSI . TBC <$> uintParam
dispatchCsi 'h' = return $ CSI SM -- private: ?
dispatchCsi 'l' = return $ CSI RM -- private: ?
dispatchCsi 'm' = CSI . SGR . SGR.dispatchSGR . (^.params) <$> get
dispatchCsi 'n' = return $ CSI DSR -- private: ' '
dispatchCsi 'r' = CSI <$> (DECSTBM <$> uintParam <*> uintParam)
dispatchCsi 's' = CSI <$> (SCP <$> uintParam <*> uintParam)
dispatchCsi x   = do
    params <- use params
    intermediate <- use intermediate
    return . Debug . T.pack $ printf
        "Unrecognised CSI: %i; params = {%s}; intermediate = {%s}"
        x (show params) (show intermediate)

init :: ParserState
init = ParserState A.Ground [] [] []

{-
applyAction :: Word.Word8 -> A.Action -> State ParserState ()
applyAction _ A.Clear = do
    params .= mempty
    intermediate .= mempty
    final .= mempty

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

transitionAction :: Word.Word8 -> A.State -> A.State -> (A.State -> A.Action) -> State (Maybe Word.Word8) ()
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
-}
