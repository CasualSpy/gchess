module Game where

import Board
import Data.Array.IArray

data Game = Game
  { currentPlayer :: Color,
    board :: Board
  }

initialGame :: Game
initialGame = Game {currentPlayer = White, board = listArray ((A, One), (H, Eight)) pieceList}
  where
    pieceList :: [Maybe Piece]
    pieceList = whitePieces ++ replicate 32 Nothing ++ blackPieces

    pieceOrder :: [PieceType]
    pieceOrder = [UnmovedRook, Knight, Bishop, Queen, UnmovedKing, Bishop, Knight, UnmovedRook]

    whitePieces :: [Maybe Piece]
    whitePieces = fmap (maybePiece White) $ pieceOrder ++ replicate 8 UnmovedPawn

    blackPieces :: [Maybe Piece]
    blackPieces = fmap (maybePiece Black) $ replicate 8 UnmovedPawn ++ pieceOrder
