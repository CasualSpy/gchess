module Board (PieceType (..), EnPassantStatus (..), Color (..), Piece (..), Rank (..), File (..), Square, Board, maybePiece, opponentOf, pieceAt) where

import Data.Array.IArray

data PieceType
  = MovedPawn EnPassantStatus -- EnPassantStatus tracks whether a pawn can be taken En Passant
  | UnmovedPawn -- Unmoved pawns can move differently from moved pawns
  | Bishop
  | Knight
  | Queen
  | UnmovedKing -- Unmoved kings can castle
  | MovedKing
  | UnmovedRook -- Castling requires the rook to never have moved
  | MovedRook
  deriving (Eq, Show)

newtype EnPassantStatus = EnPassantStatus Bool deriving (Eq, Show)

data Color = Black | White deriving (Eq, Show)

data Piece = Piece
  { color :: Color,
    pieceType :: PieceType
  }
  deriving (Eq, Show)

data File = A | B | C | D | E | F | G | H deriving (Show, Eq, Enum, Ord, Ix)

data Rank = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Enum, Ord, Ix)

type Square = (File, Rank)

type Board = Array Square (Maybe Piece)

maybePiece :: Color -> PieceType -> Maybe Piece
maybePiece c = Just . Piece c

opponentOf :: Color -> Color
opponentOf White = Black
opponentOf Black = White

pieceAt :: Board -> Square -> Maybe Piece
pieceAt b sq = b ! sq
