module TermEmu.Automata
    ( State  (..)
    , Action (..)
    , entryAction
    , exitAction
    , nextState
    ) where

import qualified Data.Word as Word

data State =
      Ground
    | Escape
    | EscapeIntermediate
    | DcsEntry
    | DcsIntermediate
    | DcsIgnore
    | DcsParam
    | DcsPassthrough
    | CsiEntry
    | CsiIntermediate
    | CsiIgnore
    | CsiParam
    | IgnoreString
    | OscString
    deriving (Show, Eq)

data Action =
      Clear
    | Execute
    | Hook
    | Unhook
    | OscStart
    | OscEnd
    | EscapeDispatch
    | CsiDispatch
    | Print
    | Collect
    | Put
    | OscPut
    | Param
    | Nop
    deriving (Show, Eq)

entryAction :: State -> Action
entryAction Escape          = Clear
entryAction DcsEntry        = Clear
entryAction DcsPassthrough  = Hook
entryAction OscString       = OscStart
entryAction CsiEntry        = Clear
entryAction _               = Nop

exitAction :: State -> Action
exitAction DcsPassthrough = Unhook
exitAction OscString      = OscEnd

inRange :: (Num a, Ord a) => a -> a -> a -> Bool
inRange x low high = low <= x && x <= high

isNonPrinting :: Word.Word8 -> Bool
isNonPrinting x =
    (x `inRange` 0x00 $ 0x17) || x == 0x19 || (x `inRange` 0x1C $ 0x1F)

isPrinting :: Word.Word8 -> Bool
isPrinting x = x `inRange` 0x20 $ 0x7F

nextState :: State -> Word.Word8 -> (State, Action)
-- From anywhere transitions
nextState _ x
    -- Ground
    | x == 0x18, x == 0x1A, x == 0x99, x == 0x9A        = (Ground, Execute)
    | x `inRange` 0x80 $ 0x8F, x `inRange` 0x91 $ 0x97  = (Ground, Execute)
    | x == 0x9C                                         = (Ground, Nop)
    -- Escape
    | x == 0x1B                                         = (Escape, Nop)
    -- IgnoreString
    | x == 0x98, x == 0x9E, x == 0x9F                   = (IgnoreString, Nop)
    -- DcsEntry
    | x == 0x90                                         = (DcsEntry, Nop)
    -- OscString
    | x == 0x9D                                         = (OscString, Nop)
    -- CsiEntry
    | x == 0x9B                                         = (CsiEntry, Nop)
nextState Ground x
    | isNonPrinting x                                   = (Ground, Execute)
    | isPrinting x                                      = (Ground, Print)
nextState EscapeIntermediate x
    | isNonPrinting x                                   = (EscapeIntermediate, Execute)
    | x `inRange` 0x20 $ 0x2F                           = (EscapeIntermediate, Collect)
    | x == 0x7F                                         = (EscapeIntermediate, Nop)
    | x `inRange` 0x30 $ 0x7E                           = (Ground, EscapeDispatch)
nextState Escape x
    | isPrinting x                                      = (Escape, Execute)
    | x == 0x7F                                         = (Escape, Nop)
    | x == 0x58, x == 0x5E, x == 0x5F                   = (IgnoreString, Nop)
    | x == 0x50                                         = (DcsEntry, Nop)
    | x == 0x5D                                         = (OscString, Nop)
    | x == 0x5B                                         = (CsiEntry, Nop)
    | x `inRange` 0x30 $ 0x4F
    , x `inRange` 0x51 $ 0x57
    , x `inRange` 0x60 $ 0x7E
    , x == 0x59, x == 0x5A, x == 0x5C                   = (Ground, EscapeDispatch)
nextState IgnoreString x
    | isNonPrinting x, isPrinting x                     = (IgnoreString, Nop)
    | x == 0x9C                                         = (Ground, Nop)
nextState DcsEntry x
    | isNonPrinting x                                   = (DcsEntry, Nop)
    | x == 0x7F                                         = (DcsEntry, Nop)
    | x `inRange` 0x20 $ 0x2F                           = (DcsIntermediate, Collect)
    | x == 0x3A                                         = (DcsIgnore, Nop)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (DcsParam, Param)
    | x `inRange` 0x3C $ 0x3F                           = (DcsParam, Collect)
    | x `inRange` 0x40 $ 0x7E                           = (DcsPassthrough, Nop)
nextState DcsIntermediate x
    | isNonPrinting x                                   = (DcsIntermediate, Nop)
    | x `inRange` 0x20 $ 0x2F                           = (DcsIntermediate, Collect)
    | x == 0x7F                                         = (DcsIntermediate, Nop)
    | x `inRange` 0x30 $ 0x3F                           = (DcsIgnore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (DcsPassthrough, Nop)
nextState DcsIgnore x
    | isNonPrinting x, isPrinting x                     = (DcsIgnore, Nop)
    | x == 0x9C                                         = (Ground, Nop)
nextState DcsParam x
    | isNonPrinting x                                   = (DcsParam, Nop)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (DcsParam, Param)
    | x == 0x7F                                         = (DcsParam, Nop)
    | x `inRange` 0x3C $ 0x3F, x == 0x3A                = (DcsIgnore, Nop)
    | x `inRange` 0x20 $ 0x2F                           = (DcsIntermediate, Collect)
    | x `inRange` 0x40 $ 0x7E                           = (DcsPassthrough, Nop)
nextState DcsPassthrough x
    | isNonPrinting x, isPrinting x                     = (DcsPassthrough, Put)
    | x == 0x7F                                         = (DcsPassthrough, Nop)
    | x == 0x9C                                         = (Ground, Nop)
nextState OscString x
    | isNonPrinting x                                   = (OscString, Nop)
    | x `inRange` 0x20 $ 0x7F                           = (OscString, OscPut)
    | x == 0x9C                                         = (Ground, Nop)
nextState CsiEntry x
    | isNonPrinting x                                   = (CsiEntry, Execute)
    | x == 0x7F                                         = (CsiEntry, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, CsiDispatch)
    | x `inRange` 0x20 $ 0x2F                           = (CsiIntermediate, Collect)
    | x == 0x3A                                         = (CsiIgnore, Nop)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (CsiParam, Param)
    | x `inRange` 0x3C $ 0x3F                           = (CsiParam, Collect)
nextState CsiIntermediate x
    | isNonPrinting x                                   = (CsiIntermediate, Execute)
    | x `inRange` 0x20 $ 0x2F                           = (CsiIntermediate, Collect)
    | x == 0x7F                                         = (CsiIntermediate, Nop)
    | x `inRange` 0x30 $ 0x3F                           = (CsiIgnore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, CsiDispatch)
nextState CsiIgnore x
    | isNonPrinting x                                   = (CsiIgnore, Execute)
    | x `inRange` 0x20 $ 0x3F, x == 0x7F                = (CsiIgnore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, Nop)
nextState CsiParam x
    | isNonPrinting x                                   = (CsiParam, Execute)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (CsiParam, Param)
    | x == 0x7F                                         = (CsiParam, Nop)
    | x `inRange` 0x3C $ 0x3F, x == 0x3A                = (CsiIgnore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, CsiDispatch)
