module Moves where

import Board (Piece, Square)

data MoveType
  = UKingMv UnmovedKingMove
  | MKingMv MovedKingMove
  | MPawnMv MovedPawnMove
  | UPawnMv UnmovedPawnMove
  | RookMv StraightMove
  | BishopMv DiagonalMove
  | KnightMv KnightMove
  | QueenMv QueenMove
  deriving (Show)

-- Some datatypes reference a direction (Up, Down, etc.). The frame of reference is White's perspective

data UnmovedKingMove = CastleLong | CastleShort | MKingMove MovedKingMove deriving (Show) -- Unmoved kings can castle

data MovedKingMove = KUp | KDown | KLeft | KRight | KUpLeft | KUpRight | KDownRight | KDownLeft deriving (Show)

data UnmovedPawnMove = PawnTwo | MPawnMove UnmovedPawnMove deriving (Show) -- Unmoved pawns can move 2 squares forward

data MovedPawnMove = PawnOne | PawnAttack PawnDir deriving (Show)

data PawnDir = PLeft | PRight deriving (Show)

data StraightMove = StrUp | StrDown | StrLeft | StrRight deriving (Show)

data DiagonalMove = DiagUpLeft | DiagUpRight | DiagDownLeft | DiagDownRight deriving (Show)

data KnightMove = NUpLeft | NUpRight | NRightUp | NRightDown | NDownRight | NDownLeft | NLeftDown | NLeftUp deriving (Show)

data QueenMove = QueenStraight StraightMove | QueenDiagonal DiagonalMove deriving (Show)

data Move = Move
  { moveType :: MoveType,
    from :: Square,
    by :: Piece
  }
  deriving (Show)
