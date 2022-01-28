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

data MovedKingMove = CastleLong | CastleShort | UKingMove KingDir

newtype UnmovedKingMove = UnmovedKingMove KingDir

data KingDir = KUp | KDown | KLeft | KRight | KUpLeft | KUpRight | KDownRight | KDownLeft

data MovedPawnMove = PawnTwo | UPawnMove UnmovedPawnMove

data UnmovedPawnMove = PawnOne | PawnAttack PawnDir

data PawnDir = PLeft | PRight

data StraightMove = StrUp Int | StrDown Int | StrLeft Int | StrRight Int

data DiagonalMove = DiagUpLeft Int | DiagUpRight Int | DiagDownLeft Int | DiagDownRight Int

data KnightMove = NUpLeft | NUpRight | NRightUp | NRightDown | NDownRight | NDownLeft | NLeftDown | NLeftUp

data QueenMove = QueenStraight StraightMove | QueenDiagonal DiagonalMove

data Move = Move
  { moveType :: MoveType,
    from :: Square
  }
