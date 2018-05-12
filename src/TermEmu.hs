module TermEmu
    ( Out(..)
    , TabClear(..)
    , BottomMargin(..)
    , CSI(..)
    , Control(..)
    , number
    , params
    , control
    , parse
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified TermEmu.SGR as SGR

import Control.Applicative ((<|>), liftA2)
import Control.Monad (join)
import Data.Attoparsec.ByteString
    (Parser, (<?>), sepBy1, string, option, many1, satisfy, inClass)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, catMaybes)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Safe (headMay)

data TabClear =
      AtCursor
    | All
    deriving (Eq, Show)

instance Enum TabClear where
    toEnum 0 = AtCursor
    toEnum 3 = All
    toEnum _ = AtCursor

    fromEnum AtCursor = 0
    fromEnum All      = 3

data BottomMargin =
      Bottom
    | Line !Natural
    deriving (Eq, Show)

data Mode =
      IRM
      | DECCKM
      | DECCOLM
      | DECAWM
      | Blinking
      | TCEM
      | Alternate !Bool
      | MouseStandard
      | MouseButton
      | MouseAll
      | MouseFocusOn
      | MouseUTF8
      | MouseSGR
    deriving (Eq, Show)

data CSI =
      ICH !Natural
    | CUU !Natural
    | CUD !Natural
    | CUF !Natural
    | CUB !Natural
    | CNL !Natural
    | CPL !Natural
    | CHA !Natural
    | CUP !Natural !Natural
    | ED  !Natural
    | EL  !Natural
    | IL  !Natural
    | DL  !Natural
    | DCH !Natural
    | SU  !Natural
    | ECH !Natural
    | CBT !Natural
    | REP !Natural
    | DA
    | VPA !Natural
    | HVP !Natural !Natural
    | TBC !TabClear
    | SM  ![Mode]
    | RM  ![Mode]
    | SGR ![Either T.Text SGR.SGR]
    | DSR
    | DECSTBM !Natural !BottomMargin
    deriving (Eq, Show)

number :: Parser Natural
number = foldl' (\ acc x -> acc * 10 + x) 0 <$> many1 digit
  where
    digit :: Parser Natural
    digit = fromIntegral . flip (-) 0x30 <$> satisfy (inClass "0-9")

-- TODO: might be different handling for ; and :
params :: Parser [Maybe Natural]
params =
    liftA2 (:) optional (separator *> optional `sepBy1` separator)
    <|> return . Just <$> param
    <|> pure []
    <?> "Params"
  where
    -- TODO: This is actually capable of taking 0x30 to 0x3F but I don't know how to
    -- deal with the non-numerics
    param = number <?> "Param"
    optional = option Nothing (Just <$> param)
    separator = AC.char ':' <|> AC.char ';'

pair :: (Natural -> Natural -> a) -> Natural -> Natural -> [Maybe Natural] -> a
pair f first second []          = f first second
pair f first second [x]         = f (fromMaybe first x) second
pair f first second (x : y : _) = f (fromMaybe first x) (fromMaybe second y)

deviceAttrs :: Bool -> [Maybe Natural] -> CSI
deviceAttrs _ _ = DA

mode :: Bool -> Natural -> Mode
mode False   4 = IRM
mode True    1 = DECCKM
mode True    3 = DECCOLM
mode True    7 = DECAWM
mode True   12 = Blinking
mode True   25 = TCEM
mode True   47 = Alternate False
mode True 1000 = MouseStandard
mode True 1002 = MouseButton
mode True 1003 = MouseAll
mode True 1004 = MouseFocusOn
mode True 1005 = MouseUTF8
mode True 1006 = MouseSGR
mode True 1047 = Alternate False
mode True 1049 = Alternate True

selectGfx :: [Maybe Natural] -> CSI
selectGfx = SGR . SGR.dispatchSGR . fmap (fromMaybe 0)

deviceStatus :: Bool -> [Maybe Natural] -> CSI
deviceStatus _ _ = DSR

setTBM :: [Maybe Natural] -> CSI
setTBM []          = DECSTBM 1 Bottom
setTBM [x]         = DECSTBM (fromMaybe 1 x) Bottom
setTBM (t : b : _) = DECSTBM (fromMaybe 1 t) (fromMaybe Bottom (Line <$> b))

csi :: Parser CSI
csi = intro *> (
        ICH <$> oneParam '@' 1
    <|> CUU <$> oneParam 'A' 1
    <|> CUD <$> oneParam 'B' 1
    <|> CUF <$> oneParam 'C' 1
    <|> CUB <$> oneParam 'D' 1
    <|> CNL <$> oneParam 'E' 1
    <|> CPL <$> oneParam 'F' 1
    <|> CHA <$> oneParam 'G' 1
    <|> pair CUP 1 2 <$> (params <* AC.char 'H')
    <|> ED  <$> oneParam 'J' 0
    <|> EL  <$> oneParam 'K' 0
    <|> IL  <$> oneParam 'L' 1
    <|> DL  <$> oneParam 'M' 1
    <|> DCH <$> oneParam 'P' 1
    <|> SU  <$> oneParam 'S' 1
    <|> ECH <$> oneParam 'X' 1
    <|> CBT <$> oneParam 'Z' 1
    <|> REP <$> oneParam 'b' 1
    <|> deviceAttrs <$> private <*> params <* AC.char 'c' -- TODO: might actually be '>' for this one's private char
    <|> VPA <$> oneParam 'd' 1
    <|> pair HVP 1 2 <$> (params <* AC.char 'f') -- TODO: consider emitting CUP here
    <|> TBC . toEnum . fromIntegral <$> oneParam 'g' 0
    <|> RM <$> (modes <* AC.char 'h')
    <|> SM <$> (modes <* AC.char 'l')
    <|> selectGfx <$> params <* AC.char 'm'
    <|> deviceStatus <$> private <*> params <* AC.char 'n'
    <|> setTBM <$> params <* AC.char 'r'
    ) <?> "CSI"
  where
    private = option False (const True <$> AC.char '?') <?> "Private"
    intro = string (BS.pack [0x1b, 0x5b]) <?> "CSI intro" -- ESC [
    defaultParam val = fromMaybe val . join . headMay <$> params
    oneParam char def = defaultParam def <* AC.char char
    modes = fmap <$> (mode <$> private) <*> (catMaybes <$> params)

raw :: Parser Word8
raw = satisfy (\ x -> x >= 0x20 && x <= 0x7F)

data Control =
      Bell
    | Backspace
    | HorizontalTab
    | LineFeed
    | CarriageReturn
    | ShiftOut
    | ShiftIn
    deriving (Eq, Show)

control :: Parser Control
control =
        Bell `recognise` '\BEL'
    <|> Backspace `recognise` '\BS'
    <|> HorizontalTab `recognise` '\HT'
    <|> LineFeed `recognise` '\LF'
    <|> LineFeed `recognise` '\VT'
    <|> LineFeed `recognise` '\FF'
    <|> CarriageReturn `recognise` '\r'
    <|> ShiftOut `recognise` '\SO'
    <|> ShiftIn `recognise` '\SI'
  where
    recognise control char = AC.char char *> pure control

data C1 =
      SCSG0_ON
    | SCSG1_ON
    | DECSC
    | DECRC
    | DECALN
    | DECKPAM
    | DECKPNM
    | SCSG0_OFF
    | SCSG1_OFF
    | IND
    | NEL
    | HTS
    | RI
    | ST
    | RIS
    deriving (Eq, Show)

esc :: Parser C1
esc = AC.char '\ESC' *> (
        AC.char '0' *> private '(' *> pure SCSG0_ON
    <|> AC.char '0' *> private ')' *> pure SCSG1_ON
    <|> AC.char '7' *> pure DECSC
    <|> AC.char '8' *> pure DECRC
    <|> AC.char '8' *> private '#' *> pure DECALN
    <|> AC.char '=' *> pure DECKPAM
    <|> AC.char '>' *> pure DECKPNM
    <|> AC.char 'B' *> private '(' *> pure SCSG0_OFF
    <|> AC.char 'B' *> private ')' *> pure SCSG1_OFF
    <|> AC.char 'D' *> pure IND
    <|> AC.char 'E' *> pure NEL
    <|> AC.char 'H' *> pure HTS
    <|> AC.char 'M' *> pure RI
    <|> AC.char '\\'*> pure ST
    <|> AC.char 'c' *> pure RIS
    )
  where
    private char = (AC.char char *> pure True) <|> pure False

-- TODO: DSR & utf-8
-- TODO: allow for malformed sequences

data Out =
      Raw !T.Text
    | C0 !Control
    | Esc !C1
    | CSI !CSI
    | Debug !T.Text
    deriving (Eq, Show)

parse :: Parser Out
parse =
        CSI <$> csi
    <|> C0 <$> control
    <|> Esc <$> esc
    <|> Raw . TE.decodeUtf8 . BS.pack <$> many1 raw
