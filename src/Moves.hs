module Moves where

import Board (Square)

data MoveType
  = MKingMv MovedKingMove
  | UKingMv UnmovedKingMove
  | MPawnMv MovedPawnMove
  | UPawnMv UnmovedPawnMove
  | RookMv StraightMove
  | BishopMv DiagonalMove
  | KnightMv KnightMove
  | QueenMv QueenMove

data UnmovedKingMove = CastleLong | CastleShort | MKingMove KingDir -- Unmoved kings can castle

newtype MovedKingMove = UnmovedKingMove KingDir

data KingDir = KUp | KDown | KLeft | KRight | KUpLeft | KUpRight | KDownRight | KDownLeft

data UnmovedPawnMove = PawnTwo | MPawnMove UnmovedPawnMove -- Unmoved pawns can move 2 squares forward

data MovedPawnMove = PawnOne | PawnAttack PawnDir

data PawnDir = PLeft | PRight

data StraightMove = StrUp Int | StrDown Int | StrLeft Int | StrRight Int

data DiagonalMove = DiagUpLeft Int | DiagUpRight Int | DiagDownLeft Int | DiagDownRight Int

data KnightMove = NUpLeft | NUpRight | NRightUp | NRightDown | NDownRight | NDownLeft | NLeftDown | NLeftUp

data QueenMove = QueenStraight StraightMove | QueenDiagonal DiagonalMove

data Move = Move
  { moveType :: MoveType,
    from :: Square
  }
