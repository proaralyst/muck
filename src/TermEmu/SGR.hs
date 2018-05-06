module TermEmu.SGR
    ( Colour8(..)
    , SGR(..)
    , dispatchSGR
    ) where

import qualified Data.Word as Word
import qualified Data.Text as T
import Control.Monad.Free (Free(..), liftF)
import Control.Lens
import Data.Maybe (fromMaybe)
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

dispatchColour8 :: Word.Word8 -> Colour8
dispatchColour8 0 = Black
dispatchColour8 1 = Red
dispatchColour8 2 = Green
dispatchColour8 3 = Yellow
dispatchColour8 4 = Blue
dispatchColour8 5 = Magenta
dispatchColour8 6 = Cyan
dispatchColour8 7 = White

data ColourRGB =
    ColourRGB
    { _red :: Natural
    , _green :: Natural
    , _blue :: Natural
    } deriving (Eq, Ord, Show)

makeLenses ''ColourRGB

data Colour =
      Colour8 Colour8
    | Colour256 Word.Word8
    | RGB ColourRGB
    | Default
    deriving (Eq, Show)

data SGR =
      Bright
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
    | Pop (Maybe Word.Word8 -> r)
    deriving (Functor)

type Effect = Free EffectF

emit :: SGR -> Effect ()
emit sgr = liftF (Emit sgr ())

-- TODO: rename
throwError :: T.Text -> Effect ()
throwError text = liftF (Error text ())

pop :: Effect (Maybe Word.Word8)
pop = liftF (Pop id)

consumeColourRgb :: Effect ColourRGB
consumeColourRgb = do
    let defaultPop = fromIntegral . fromMaybe 0 <$> pop
    ColourRGB <$> defaultPop <*> defaultPop <*> defaultPop

consumeColour :: (Colour -> SGR) -> Effect ()
consumeColour f =
    pop >>= \case
        Just 2 -> f . RGB <$> consumeColourRgb >>= emit
        Just 5 -> f . Colour256 . fromMaybe 0 <$> pop >>= emit
        Just x -> throwError . T.pack $
            printf "Unrecognised extended colour prefix: %i" x
        Nothing -> return ()

-- TODO: extended colour & parameter management
consumeParam :: Word.Word8 -> Effect ()
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
    emit . Fg . Colour8 . dispatchColour8 $ x - 30
    | x >= 40 && x <= 47 =
    emit . Bg . Colour8 . dispatchColour8 $ x - 40
    | otherwise =
        throwError . T.pack $ printf "Unrecognised SGR param: %i" x

dispatch :: Effect ()
dispatch =
    pop >>= \case
    Nothing -> return ()
    Just p -> do
        consumeParam p
        dispatch

runEffect :: [Word.Word8] -> Effect a -> ([Either T.Text SGR], a)
runEffect = run [] where
    run acc _ (Pure x) = (reverse acc, x)
    run acc params (Free f) = case f of
        Emit x r -> run (Right x : acc) params r
        Error x r -> run (Left x : acc) params r
        Pop f -> case params of
            []     -> run acc params (f Nothing)
            (p:ps) -> run acc ps     (f $ Just p)

-- TODO: given this is processing a stream, maybe another type?
dispatchSGR :: [Word.Word8] -> [Either T.Text SGR]
dispatchSGR params = fst $ runEffect params dispatch
