module TermEmu
    ( Out(..)
    , TabClear(..)
    , BottomMargin(..)
    , CSI(..)
    , number
    , params
    , parse
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified TermEmu.SGR as SGR

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
    (Parser, (<?>), sepBy, string, option, many1, satisfy, inClass)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Safe (headMay)

-- TODO: DA SM RM & DSR

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
    | SM
    | RM
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
params :: Parser [Natural]
params = param `sepBy` separator <?> "Params"
  where
    -- TODO: This is actually capable of taking 0x30 to 0x3F but I don't know how to
    -- deal with the non-numerics
    param :: Parser Natural
    param = number <?> "Param"
    separator = AC.char ':' <|> AC.char ';'

pair :: (Natural -> Natural -> a) -> Natural -> Natural -> [Natural] -> a
pair f first second []          = f first second
pair f _     second [x]         = f x second
pair f _     _      (x : y : _) = f x y

deviceAttrs :: Bool -> [Natural] -> CSI
deviceAttrs = undefined

resetMode :: Bool -> [Natural] -> CSI
resetMode = undefined

setMode :: Bool -> [Natural] -> CSI
setMode = undefined

selectGfx :: [Natural] -> CSI
selectGfx = SGR . SGR.dispatchSGR

deviceStatus :: Bool -> [Natural] -> CSI
deviceStatus = undefined

setTBM :: [Natural] -> CSI
setTBM [] = DECSTBM 1 Bottom
setTBM [x] = DECSTBM x Bottom
setTBM (t : b : _) = DECSTBM t (Line b)

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
    <|> resetMode <$> private <*> params <* AC.char 'h'
    <|>   setMode <$> private <*> params <* AC.char 'l'
    <|> selectGfx <$> params <* AC.char 'm'
    <|> deviceStatus <$> private <*> params <* AC.char 'n'
    <|> setTBM <$> params <* AC.char 'r'
    ) <?> "CSI"
  where
    private = option False (const True <$> AC.char '?') <?> "Private"
    intro = string (BS.pack [0x1b, 0x5b]) <?> "CSI intro" -- ESC [
    defaultParam val = fromMaybe val . headMay <$> params
    oneParam char def = defaultParam def <* AC.char char

raw :: Parser Word8
raw = satisfy (\ x -> x >= 0x20 && x <= 0x7F)

-- TODO: for speed convert Raw Char to Raw ByteString
data Out =
      Raw Char
    | Esc
    | CSI CSI
    | Debug T.Text
    deriving (Eq, Show)

parse :: Parser Out
parse =
    CSI <$> csi
    <|> Raw . toEnum . fromIntegral <$> raw
