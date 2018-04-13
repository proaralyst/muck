module TermEmu.Automata
    ( State  (..)
    , Action (..)
    ,
    )

import qualified Data.Word as Word

data State =
      Ground
    | Escape_immediate
    | Escape
    | Dcs_entry
    | Dcs_intermediate
    | Dcs_ignore
    | Dcs_param
    | Dcs_passthrough
    | Csi_entry
    | Csi_intermediate
    | Csi_ignore
    | Csi_param
    | Sos_pm_apc_string
    | Osc_string
    | Error
    deriving (Show)

data Action =
    | Clear
    | Execute
    | Hook
    | Osc_start
    | Nop
    | Esc_dispatch
    | Csi_dispatch
    | Print
    | Collect
    | Put
    | Osc_put
    | Param
    | Error String

entryAction :: State -> Action
entryAction Escape          = Clear
entryAction Dcs_entry       = Clear
entryAction Dcs_passthrough = Hook
entryAction Osc_string      = Osc_start
entryAction Csi_entry       = Clear
entryAction _               = Nop

exitAction :: State -> Action
exitAction Dcs_passthrough  = Unhook
entryAction Osc_string      = Osc_end

inRange :: (Num a) => a -> a -> a -> Bool
inRange x low high = low <= x && x <= high

isNonPrinting :: Word.Word8 -> Bool
isNonPrinting x =
    x `inRange` 0x00 $ 0x17, x = 0x19, x `inRange` 0x1C $ 0x1F

isPrinting :: Word.Word8 -> Bool
isPrinting x = x `inRange` 0x20 $ 0x7F

nextState :: State -> Word.Word8 -> (State, Action)
-- Can't leave the error state
nextState Error _ = (Error, Nop)
-- From anywhere transitions
nextState _ x
    -- Ground
    | x == 0x18, x == 0x1A, x == 0x99, x == 0x9A        = (Ground, Execute)
    | x `inRange` 0x80 $ 0x8F, x `inRange` $ 0x91 0x97  = (Ground, Execute)
    | x == 0x9C                                         = (Ground, Nop)
    -- Escape
    | x == 0x1B                                         = (Escape, Nop)
    -- Sos_pm_apc_string
    | x == 0x98, x == 0x9E, x == 0x9F                   = (Sos_pm_apc_string, Nop)
    -- Dcs_entry
    | x == 0x90                                         = (Dcs_entry, Nop)
    -- Osc_string
    | x == 0x9D                                         = (Osc_string, Nop)
    -- Csi_entry
    | x == 0x9B                                         = (Csi_entry, Nop)
nextState Ground x
    | isNonPrinting x                                   = (Ground, Execute)
    | isPrinting x                                      = (Ground, Print)
nextState Escape_immediate x
    | isNonPrinting x                                   = (Escape_immediate, Execute)
    | x `inRange` 0x20 $ 0x2F                           = (Escape_immediate, Collect)
    | x == 0x7F                                         = (Escape_immediate, Nop)
    | x `inRange` 0x30 $ 0x7E                           = (Ground, Esc_dispatch)
nextState Escape x
    | isPrinting x                                      = (Escape, Execute)
    | x == 0x7F                                         = (Escape, Nop)
    | x == 0x58, x == 0x5E, x == 0x5F                   = (Sos_pm_apc_string, Nop)
    | x == 0x50                                         = (Dcs_entry, Nop)
    | x == 0x5D                                         = (Osc_string, Nop)
    | x == 0x5B                                         = (Csi_entry, Nop)
    | x `inRange` 0x30 $ 0x4F
    , x `inRange` 0x51 $ 0x57
    , x `inRange` 0x60 $ 0x7E
    , x == 0x59, x == 0x5A, x == 0x5C                   = (Ground, Esc_dispatch)
nextState Sos_pm_apc_string x
    | isNonPrinting x, isPrinting x                     = (Sos_pm_apc_string, Nop)
    | x == 0x9C                                         = (Ground, Nop)
nextState Dcs_entry x
    | isNonPrinting x                                   = (Dcs_entry, Nop)
    | x                                                 = 0x7F          = (Dcs_entry, Nop)
    | x `inRange` 0x20 $ 0x2F                           = (Dcs_intermediate, Collect)
    | x == 0x3A                                         = (Dcs_ignore, Nop)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (Dcs_param, Param)
    | x `inRange` 0x3C $ 0x3F                           = (Dcs_param, Collect)
    | x `inRange` 0x40 $ 0x7E                           = (Dcs_passthrough, Nop)
nextState Dcs_intermediate x
    | isNonPrinting x                                   = (Dcs_intermediate, Nop)
    | x `inRange` 0x20 $ 0x2F                           = (Dcs_intermediate, Collect)
    | x == 0x7F                                         = (Dcs_intermediate, Nop)
    | x `inRange` 0x30 $ 0x3F                           = (Dcs_ignore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Dcs_passthrough, Nop)
nextState Dcs_ignore x
    | isNonPrinting x, isPrinting x                     = (Dcs_ignore, Nop)
    | x == 0x9C                                         = (Ground, Nop)
nextState Dcs_param x
    | isNonPrinting x                                   = (Dcs_param, Nop)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (Dcs_param, Param)
    | x == 0x7F                                         = (Dcs_param, Nop)
    | x `inRange` 0x3C $ 0x3F, x == 0x3A                = (Dcs_ignore, Nop)
    | x `inRange` 0x20 $ 0x2F                           = (Dcs_intermediate, Collect)
    | x `inRange` 0x40 $ 0x7E                           = (Dcs_passthrough, Nop)
nextState Dcs_passthrough x
    | isNonPrinting x, isPrinting x                     = (Dcs_passthrough, Put)
    | x == 0x7F                                         = (Dcs_passthrough, Nop)
    | x == 0x9C                                         = (Ground, Nop)
nextState Osc_string x
    | isNonPrinting x                                   = (Osc_string, Nop)
    | x `inRange` 0x20 $ 0x7F                           = (Osc_string, Osc_put)
    | x == 0x9C                                         = (Ground, Nop)
nextState Csi_entry x
    | isNonPrinting x                                   = (Csi_entry, Execute)
    | x == 0x7F                                         = (Csi_entry, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, Csi_dispatch)
    | x `inRange` 0x20 $ 0x2F                           = (Csi_intermediate, Collect)
    | x == 0x3A                                         = (Csi_ignore, Nop)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (Csi_param, Param)
    | x `inRange` 0x3C $ 0x3F                           = (Csi_param, Collect)
nextState Csi_intermediate x
    | isNonPrinting x                                   = (Csi_intermediate, Execute)
    | x `inRange` 0x20 $ 0x2F                           = (Csi_intermediate, Collect)
    | x == 0x7F                                         = (Csi_intermediate, Nop)
    | x `inRange` 0x30 $ 0x3F                           = (Csi_ignore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, Csi_dispatch)
nextState Csi_ignore x
    | isNonPrinting x                                   = (Csi_ignore, Execute)
    | x `inRange` 0x20 $ 0x3F, x == 0x7F                = (Csi_ignore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, Nop)
nextState Csi_param x
    | isNonPrinting x                                   = (Csi_param, Execute)
    | x `inRange` 0x30 $ 0x39, x == 0x3B                = (Csi_param, Param)
    | x == 0x7F                                         = (Csi_param, Nop)
    | x `inRange` 0x3C $ 0x3F, x == 0x3A                = (Csi_ignore, Nop)
    | x `inRange` 0x40 $ 0x7E                           = (Ground, Csi_dispatch)
