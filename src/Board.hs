module Board where

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
  deriving (Eq)

newtype EnPassantStatus = EnPassantStatus Bool deriving (Eq)

data Color = Black | White deriving (Eq)

data Piece = Piece Color PieceType deriving (Eq)
