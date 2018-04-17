module TermEmu
    ( ParserState(..)
    ) where

import Prelude hiding (print, read)

import qualified Data.Word as Word
import qualified TermEmu.Automata as A
import Control.Lens
import Control.Monad.Free (Free(..), liftF)

data CSI =
      ICH
    | CUU
    | CUD
    | CUF
    | CUB
    | CNL
    | CPL
    | HPA
    | CUP
    | ED
    | EL
    | IL
    | DL
    | DCH
    | SU
    | ECH
    | CBT
    | REP
    | DA
    | DA_TWO
    | VPA
    | TBC
    | SM
    | SM_PRIVATE
    | RM
    | RM_PRIVATE
    | SGR
    | DSR
    | DECSCUSR
    | DECSTBM
    | SCP
    | WINOPS
    | RCP

parseCsi :: Char -> String -> Maybe CSI
parseCsi '@' ""  = Just ICH
parseCsi 'A' ""  = Just CUU
parseCsi 'B' ""  = Just CUD
parseCsi 'C' ""  = Just CUF
parseCsi 'D' ""  = Just CUB
parseCsi 'E' ""  = Just CNL
parseCsi 'F' ""  = Just CPL
parseCsi 'G' ""  = Just HPA
parseCsi 'H' ""  = Just CUP
parseCsi 'J' ""  = Just ED
parseCsi 'K' ""  = Just EL
parseCsi 'L' ""  = Just IL
parseCsi 'M' ""  = Just DL
parseCsi 'P' ""  = Just DCH
parseCsi 'S' ""  = Just SU
parseCsi 'X' ""  = Just ECH
parseCsi 'Z' ""  = Just CBT
parseCsi 'b' ""  = Just REP
parseCsi 'c' ""  = Just DA
parseCsi 'c' ">" = Just DA_TWO
parseCsi 'd' ""  = Just VPA
parseCsi 'f' ""  = Just CUP
parseCsi 'g' ""  = Just TBC
parseCsi 'h' ""  = Just SM
parseCsi 'h' "?" = Just SM_PRIVATE
parseCsi 'l' ""  = Just RM
parseCsi 'l' "?" = Just RM_PRIVATE
parseCsi 'm' ""  = Just SGR
parseCsi 'n' ""  = Just DSR
parseCsi 'q' " " = Just DECSCUSR
parseCsi 'r' ""  = Just DECSTBM
parseCsi 's' ""  = Just SCP
parseCsi 't' ""  = Just WINOPS
parseCsi 'u' ""  = Just RCP
parseCsi _ _     = Nothing

data ParserState =
    ParserState
    { _automataState :: A.State
    , _params        :: [Word.Word8]
    , _intermediate  :: [Word.Word8]
    , _final         :: [Word.Word8]
    }

makeLenses ''ParserState

init :: ParserState
init = ParserState A.Ground [] [] []

data Effect next =
      Read (ParserState -> next)
    | Write ParserState next
    | Modify (ParserState -> ParserState) next
    | Print Word.Word8 next
    deriving (Functor)

type Program = Free Effect

read :: Program ParserState
read = liftF (Read id)

write :: ParserState -> Program ()
write state = liftF (Write state ())

modify :: (ParserState -> ParserState) -> Program ()
modify f = liftF (Modify f ())

print :: Word.Word8 -> Program ()
print char = liftF (Print char ())

applyAction :: Word.Word8 -> A.Action -> Program ()
applyAction _ A.Clear = do
    modify $ set params mempty
    modify $ set intermediate mempty
    modify $ set final mempty
applyAction x A.Execute =
    -- TODO: actually execute
    applyAction x A.Clear
applyAction x A.Hook =
    -- TODO: actually hook?
    applyAction x A.Clear
applyAction x A.Unhook =
    -- TODO: actually uhhook?
    applyAction x A.Clear
applyAction x A.OscStart = applyAction x A.Clear
applyAction x A.OscEnd = applyAction x A.Clear
applyAction x A.EscapeDispatch = applyAction x A.Clear
applyAction x A.CsiDispatch = applyAction x A.Clear
applyAction x A.Print = print x
applyAction x A.Collect = modify $ over intermediate (x:)
applyAction _ A.Put = return () 
applyAction _ A.OscPut = return ()
applyAction x A.Param = modify $ over params (x:)
applyAction _ A.Nop = return ()

transitionAction :: Word.Word8 -> A.State -> A.State -> (A.State -> A.Action) -> Program ()
transitionAction char oldState newState f
    | oldState == newState = applyAction char $ f newState
    | otherwise = return ()

process :: Word.Word8 -> Program ()
process char = do
    oldState <- view automataState <$> read
    let (nextState, action) = A.nextState oldState char
    let doAction = transitionAction char oldState nextState 
    doAction A.entryAction
    applyAction char action
    doAction A.exitAction
    modify $ set automataState nextState
