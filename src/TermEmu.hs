module TermEmu
    ( Term(..)
    , init
    ) where

import Prelude hiding (init)

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified TermEmu.Parser as Parser

import Control.Lens
import Control.Monad.State (State)
import Numeric.Natural (Natural)

data Term = Term
    { _width  :: !Natural
    , _height :: !Natural
    , _screen :: !(Seq.Seq T.Text)
    , _alt    :: !(Seq.Seq T.Text)
    } deriving (Show)

makeLenses ''Term

init :: Natural -> Natural -> Term
init width height = Term width height mempty mempty

data Query r =
      Resize Natural Natural r
    | Receive Parser.Out r
    deriving (Show)
      
eval :: Query a -> State Term

