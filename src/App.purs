module App where

import Prelude
import Control.Monad.State as State
import Data.Array as Array
import Data.Bounded.Generic as Bounded.Generic
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic as Enum.Generic
import Data.Generic.Rep (class Generic)
import Data.Lens ((<>~))
import Data.Lens.Index as Index
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Show.Generic as Show.Generic
import Data.Traversable (class Traversable)
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Unsafe
import React.Basic.DOM as R
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, JSX, (/\))
import React.Basic.Hooks as Hooks

data Rank
  = R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8

derive instance eqRank :: Eq Rank

derive instance ordRank :: Ord Rank

derive instance genericRank :: Generic Rank _

instance showRank :: Show Rank where
  show = case _ of
    R1 -> "1"
    R2 -> "2"
    R3 -> "3"
    R4 -> "4"
    R5 -> "5"
    R6 -> "6"
    R7 -> "7"
    R8 -> "8"

instance enumRank :: Enum Rank where
  succ = Enum.Generic.genericSucc
  pred = Enum.Generic.genericPred

instance boundedRank :: Bounded Rank where
  bottom = Bounded.Generic.genericBottom
  top = Bounded.Generic.genericTop

instance boundedEnumRank :: BoundedEnum Rank where
  cardinality = Enum.Generic.genericCardinality
  toEnum = Enum.Generic.genericToEnum
  fromEnum = Enum.Generic.genericFromEnum

data File
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H

derive instance eqFile :: Eq File

derive instance ordFile :: Ord File

derive instance genericFile :: Generic File _

instance showFile :: Show File where
  show = Show.Generic.genericShow

instance enumFile :: Enum File where
  succ = Enum.Generic.genericSucc
  pred = Enum.Generic.genericPred

instance boundedFile :: Bounded File where
  bottom = Bounded.Generic.genericBottom
  top = Bounded.Generic.genericTop

instance boundedEnumFile :: BoundedEnum File where
  cardinality = Enum.Generic.genericCardinality
  toEnum = Enum.Generic.genericToEnum
  fromEnum = Enum.Generic.genericFromEnum

newtype Square
  = Square
  { file :: File
  , rank :: Rank
  }

derive instance eqSquare :: Eq Square

derive instance ordSquare :: Ord Square

data Piece
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

data Player
  = Black
  | White

type PlayerPiece
  = { player :: Player
    , piece :: Piece
    }

type Board
  = Map Square (Maybe PlayerPiece)

renderPlayerPiece :: PlayerPiece -> JSX
renderPlayerPiece { player, piece } =
  R.span
    { className:
        case player of
          White -> "white"
          Black -> "black"
    , children:
        [ R.text case piece of
            King -> "♚"
            Queen -> "♛"
            Rook -> "♜"
            Bishop -> "♝"
            Knight -> "♞"
            Pawn -> "♟︎"
        ]
    }

renderBoard :: Board -> JSX
renderBoard =
  R.div_
    <<< map renderRank
    <$> chunksOf 8
    <<< Map.toUnfoldable

renderRank :: Array (Tuple Square (Maybe PlayerPiece)) -> JSX
renderRank =
  R.div_
    <<< map \(Tuple _ piece) ->
        R.div_ [ Maybe.maybe mempty renderPlayerPiece piece ]

-- | Completely unnecessary
chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n =
  flip State.execState [ [] ]
    <<< Traversable.traverse \x -> do
        acc <- State.get
        case Array.length <$> Array.last acc of
          Just m
            | m == n -> State.modify (_ `Array.snoc` [ x ])
          _ -> State.modify (Index.ix (Array.length acc - 1) <>~ [ x ])

mkApp :: Component Unit
mkApp = do
  Hooks.component "App" \_ -> Hooks.do
    pure do
      R.h1_ [ R.text "Hello" ]
