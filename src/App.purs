module App where

import Prelude
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.Bounded.Generic as Bounded.Generic
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum as Enum
import Data.Enum.Generic as Enum.Generic
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic as Show.Generic
import Data.Tuple (Tuple(..))
import Debug (class DebugWarning)
import Debug as Debug
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

derive instance genericPiece :: Generic Piece _

instance showPiece :: Show Piece where
  show = Show.Generic.genericShow

data Player
  = Black
  | White

derive instance genericPlayer :: Generic Player _

instance showPlayer :: Show Player where
  show = Show.Generic.genericShow

type PlayerPiece
  = { player :: Player
    , piece :: Piece
    }

type Board
  = Map Square (Maybe PlayerPiece)

mkPlayerPiece :: Component { playerPiece :: PlayerPiece, square :: Square }
mkPlayerPiece = do
  gameContext <- Reader.ask
  component "PlayerPiece" \({ playerPiece: { player, piece }, square }) -> Hooks.do
    { setHighlightedSquares, setMovingFrom, movingTo, highlightedSquares, setBoardState } <- useGameContext gameContext
    pure do
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
        , onDragStart:
            Events.handler_ do
              setHighlightedSquares (validMoves { player, piece } square)
              setMovingFrom (Just square)
        , onDragEnd:
            Events.handler_ do
              Foldable.for_ movingTo \to_ ->
                -- EWWWW
                Monoid.guard (Set.member to_ highlightedSquares)
                  (Debug.trace (show square) \_ -> setBoardState (movePiece { player, piece } square to_))
              setHighlightedSquares Set.empty
              setMovingFrom Nothing
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
          <$> chunksOf 8
          <<< Array.reverse
          <<< Map.toUnfoldable
      )
        boardState

mkRank :: Component (Array (Square /\ Maybe PlayerPiece))
mkRank = do
  gameContext <- Reader.ask
  playerPieceComponent <- mkPlayerPiece
  component "Rank" \props -> Hooks.do
    { highlightedSquares, setMovingTo } <- useGameContext gameContext
    pure do
      ( R.div <<< { className: "rank", children: _ }
          <<< map \(Tuple square maybePlayerPiece) ->
              -- Debug.trace (show { highlightedSquares, square }) \_ ->
              R.div
                { className: "square" <> Monoid.guard (Set.member square highlightedSquares) " highlighted"
                , children: [ Maybe.maybe mempty (playerPieceComponent <<< { playerPiece: _, square }) maybePlayerPiece ]
                , onDragOver:
                    Events.handler_ do
                      setMovingTo if Set.member square highlightedSquares then Just square else Nothing
                }
      )
        props

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n = go []
  where
  go acc = case _ of
    [] -> acc
    xs -> go (acc <> [ Array.take n xs ]) (Array.drop n xs)

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
    , movingFrom :: Maybe Square
    , setMovingFrom :: Maybe Square -> Effect Unit
    , movingTo :: Maybe Square
    , setMovingTo :: Maybe Square -> Effect Unit
    , highlightedSquares :: Set Square
    , setHighlightedSquares :: Set Square -> Effect Unit
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
      highlightedSquares /\ setHighlightedSquares <- Hooks.useState' Set.empty
      movingFrom /\ setMovingFrom <- Hooks.useState' Nothing
      movingTo /\ setMovingTo <- Hooks.useState' Nothing
      Debug.traceM (show movingTo)
      pure do
        Hooks.provider gameContext
          ( Just
              { boardState
              , setBoardState
              , currentPlayer
              , switchCurrentPlayer
              , highlightedSquares
              , setHighlightedSquares
              , movingFrom
              , setMovingFrom
              , movingTo
              , setMovingTo
              }
          )
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

validMoves :: PlayerPiece -> Square -> Set Square
validMoves { player, piece } (Square { rank, file }) =
  Set.fromFoldable case piece of
    Pawn -> case player, rank of
      White, R2 ->
        Array.catMaybes
          [ Square <<< { file, rank: _ } <$> (Enum.succ >=> Enum.succ) rank
          , Square <<< { file, rank: _ } <$> Enum.succ rank
          ]
      White, _ ->
        Array.catMaybes
          [ Square <<< { file, rank: _ } <$> Enum.succ rank
          ]
      Black, R7 ->
        Array.catMaybes
          [ Square <<< { file, rank: _ } <$> (Enum.pred >=> Enum.pred) rank
          , Square <<< { file, rank: _ } <$> Enum.pred rank
          ]
      Black, _ ->
        Array.catMaybes
          [ Square <<< { file, rank: _ } <$> Enum.pred rank
          ]
    _ -> []

movePiece :: PlayerPiece -> Square -> Square -> Board -> Board
movePiece playerPiece from to = Map.union (Map.fromFoldable [ Tuple from Nothing, Tuple to (Just playerPiece) ])
