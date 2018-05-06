module TermEmu
    ( Out(..)
    , number
    , params
    , parse
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
    (Parser(..) , (<?>), sepBy, string, option, many1, satisfy, inClass)
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.Foldable (foldl')
import Numeric.Natural (Natural)

-- TODO: DA SM RM & DSR

type Param = Natural

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
    | SGR
    | DSR
    | DECSTBM Param Param
    | SCP Param Param
    deriving (Show)

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


csi :: Parser CSI
csi = undefined
  where
    private = option False $ const True <$> AC.char '?'
    intro = string (BS.pack [0x1b, 0x5b]) <?> "CSI" -- ESC [

data Out =
      Raw Char
    | Esc
    | CSI CSI
    | Debug T.Text
    deriving (Show)


parse :: Parser Out
parse = undefined
