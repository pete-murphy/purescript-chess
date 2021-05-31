module App where

import Prelude
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.Bounded.Generic as Bounded.Generic
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum as Enum
import Data.Enum.Generic as Enum.Generic
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Show.Generic as Show.Generic
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Partial.Unsafe as Partial.Unsafe
import React.Basic.DOM as R
import React.Basic.Events as Events
import React.Basic.Hooks (type (/\), Hook, JSX, ReactContext, Render, UseContext, UseState, (/\))
import React.Basic.Hooks as Hooks

type ComponentM env props
  = ReaderT env Effect (props -> JSX)

component ::
  forall env props hooks.
  String -> (props -> Render Unit hooks JSX) -> ComponentM env props
component name render = ReaderT \_ -> Hooks.component name render

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

instance showSquare :: Show Square where
  show (Square { file, rank }) = show file <> show rank

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
    , draggable: true
    }

mkBoard :: Component Unit
mkBoard = do
  gameContext <- Reader.ask
  rank <- mkRank
  component "Board" \_ -> Hooks.do
    { boardState } <- useGameContext gameContext
    pure do
      ( R.div <<< { className: "board", children: _ }
          <<< map rank
          <<< Array.reverse
          <$> chunksOf' 8
          <<< Array.reverse
          <<< Map.toUnfoldable
      )
        boardState

mkRank :: Component (Array (Square /\ Maybe PlayerPiece))
mkRank =
  component "Rank" \props ->
    pure do
      ( R.div <<< { className: "rank", children: _ }
          <<< map \(Tuple x piece) ->
              R.div
                { className: "square"
                , children: [ Maybe.maybe mempty renderPlayerPiece piece ]
                , onDragOver: Events.handler_ (Console.logShow x)
                }
      )
        props

chunksOf' :: forall a. Int -> Array a -> Array (Array a)
chunksOf' n = go []
  where
  go acc [] = acc

  go acc xs = go (acc <> [ Array.take n xs ]) (Array.drop n xs)

initialBoard :: Board
initialBoard = Map.fromFoldable (allSquares `Array.zip` allPieces)
  where
  allSquares = do
    rank <- bottom `Enum.enumFromTo` top
    file <- bottom `Enum.enumFromTo` top
    pure (Square { rank, file })

  allPieces =
    Array.concat do
      [ Just <<< { player: White, piece: _ } <$> backRank
      , Just <<< { player: White, piece: _ } <$> Array.replicate 8 Pawn
      , Array.replicate 32 Nothing
      , Just <<< { player: Black, piece: _ } <$> Array.replicate 8 Pawn
      , Just <<< { player: Black, piece: _ } <$> backRank
      ]

  backRank = [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ]

type GameState
  = { boardState :: Board
    , setBoardState :: (Board -> Board) -> Effect Unit
    , currentPlayer :: Player
    , switchCurrentPlayer :: Effect Unit
    }

type GameContext
  = ReactContext (Maybe GameState)

type Component props
  = ComponentM GameContext props

mkGameContext :: Effect (GameContext)
mkGameContext = Hooks.createContext Nothing

useGameContext :: GameContext -> Hook (UseContext (Maybe GameState)) GameState
useGameContext gameContext = Hooks.do
  maybeContextValue <- Hooks.useContext gameContext
  pure case maybeContextValue of
    Nothing ->
      Partial.Unsafe.unsafeCrashWith
        "useContext can only be used in a descendant of \
        \the corresponding context provider component"
    Just contextValue -> contextValue

mkApp :: Hooks.Component Unit
mkApp = do
  gameContext <- mkGameContext
  flip Reader.runReaderT gameContext do
    board <- mkBoard
    component "App" \_ -> Hooks.do
      boardState /\ setBoardState <- Hooks.useState initialBoard
      currentPlayer /\ switchCurrentPlayer <- useCurrentPlayer
      pure do
        Hooks.provider gameContext (Just { boardState, setBoardState, currentPlayer, switchCurrentPlayer })
          [ board unit ]

useCurrentPlayer :: Hook (UseState Player) (Player /\ Effect Unit)
useCurrentPlayer = Hooks.do
  currentPlayer /\ setCurrentPlayer <- Hooks.useState White
  let
    switchCurrentPlayer =
      setCurrentPlayer case _ of
        White -> Black
        Black -> White
  pure (currentPlayer /\ switchCurrentPlayer)
