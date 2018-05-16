module TermEmu.Parser.SGR
    ( Colour8(..)
    , ColourRGB(..), red , green, blue
    , Colour(..)
    , SGR(..)
    , dispatchSGR
    ) where

import qualified Data.Text as T
import Control.Monad.Free (Free(..), liftF)
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Text.Printf (printf)

data Colour8 =
      Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    deriving (Eq, Ord, Show)

instance Enum Colour8 where
    toEnum 0 = Black
    toEnum 1 = Red
    toEnum 2 = Green
    toEnum 3 = Yellow
    toEnum 4 = Blue
    toEnum 5 = Magenta
    toEnum 6 = Cyan
    toEnum 7 = White
    toEnum _ = White

    fromEnum Black   = 0
    fromEnum Red     = 1
    fromEnum Green   = 2
    fromEnum Yellow  = 3
    fromEnum Blue    = 4
    fromEnum Magenta = 5
    fromEnum Cyan    = 6
    fromEnum White   = 7

data ColourRGB =
    ColourRGB
    { _red :: Natural
    , _green :: Natural
    , _blue :: Natural
    } deriving (Eq, Ord, Show)

makeLenses ''ColourRGB

data Colour =
      Colour8 Colour8
    | Colour256 Word8
    | RGB ColourRGB
    | Default
    deriving (Eq, Show)

data SGR =
      Clear
    | Bright
    | Dim
    | Italics
    | Underscore
    | Blink
    | Reverse
    | Hidden
    | Strikethrough
    | NormalIntensity
    | Roman
    | NotUnderscore
    | Steady
    | Positive
    | Revealed
    | NotStrikethrough
    | Fg Colour
    | Bg Colour
    deriving (Eq, Show)

data EffectF r =
      Emit SGR r
    | Error T.Text r
    | Pop (Maybe Natural -> r)
    deriving (Functor)

type Effect = Free EffectF

emit :: SGR -> Effect ()
emit sgr = liftF (Emit sgr ())

-- TODO: rename
throwError :: T.Text -> Effect ()
throwError text = liftF (Error text ())

pop :: Effect (Maybe Natural)
pop = liftF (Pop id)

consumeColourRgb :: Effect ColourRGB
consumeColourRgb = do
    let defaultPop = fromIntegral . fromMaybe 0 <$> pop
    ColourRGB <$> defaultPop <*> defaultPop <*> defaultPop

consumeColour :: (Colour -> SGR) -> Effect ()
consumeColour f =
    pop >>= \case
        Just 2 -> f . RGB <$> consumeColourRgb >>= emit
        Just 5 -> f . Colour256 . fromIntegral . fromMaybe 0 <$> pop >>= emit
        Just x -> throwError . T.pack $
            printf "Unrecognised extended colour prefix: %i" x
        Nothing -> return ()

-- TODO: extended colour & parameter management
consumeParam :: Natural -> Effect ()
consumeParam 0  = emit Clear
consumeParam 1  = emit Bright
consumeParam 2  = emit Dim
consumeParam 3  = emit Italics
consumeParam 4  = emit Underscore
consumeParam 5  = emit Blink
consumeParam 7  = emit Reverse
consumeParam 8  = emit Hidden
consumeParam 9  = emit Strikethrough
consumeParam 22 = emit NormalIntensity
consumeParam 23 = emit Roman
consumeParam 24 = emit NotUnderscore
consumeParam 25 = emit Steady
consumeParam 26 = emit Positive
consumeParam 27 = emit Revealed
consumeParam 29 = emit NotStrikethrough
consumeParam 38 = consumeColour Fg
consumeParam 39 = emit $ Fg Default
consumeParam 48 = consumeColour Bg
consumeParam 49 = emit $ Bg Default
consumeParam x
    | x >= 30 && x <= 37 =
    emit . Fg . Colour8 . toEnum . fromIntegral $ x - 30
    | x >= 40 && x <= 47 =
    emit . Bg . Colour8 . toEnum . fromIntegral $ x - 40
    | otherwise =
        throwError . T.pack $ printf "Unrecognised SGR param: %i" x

dispatch :: Effect ()
dispatch =
    pop >>= \case
    Nothing -> return ()
    Just p -> do
        consumeParam p
        dispatch

runEffect :: [Natural] -> Effect a -> ([Either T.Text SGR], a)
runEffect = run [] where
    run acc _ (Pure x) = (reverse acc, x)
    run acc params (Free f) = case f of
        Emit x r -> run (Right x : acc) params r
        Error x r -> run (Left x : acc) params r
        Pop f -> case params of
            []     -> run acc params (f Nothing)
            (p:ps) -> run acc ps     (f $ Just p)

-- TODO: given this is processing a stream, maybe another type?
dispatchSGR :: [Natural] -> [Either T.Text SGR]
dispatchSGR params = fst $ runEffect params dispatch
