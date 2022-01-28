{-# LANGUAGE OverloadedLists #-}

module Game where

import Board
import Data.Array.IArray
import qualified Data.Sequence as S
import Moves

data Game = Game
  { currentPlayer :: Color,
    gameBoard :: Board
  }

initialGame :: Game
initialGame = Game {currentPlayer = White, gameBoard = listArray ((A, One), (H, Eight)) pieceList}
  where
    pieceList :: [Maybe Piece]
    pieceList = pieceOrder >>= mkFile

    mkFile :: PieceType -> [Maybe Piece]
    mkFile pt = [Just $ Piece White pt, Just $ Piece White UnmovedPawn, Nothing, Nothing, Nothing, Nothing, Just $ Piece Black UnmovedPawn, Just $ Piece Black pt]

    pieceOrder :: [PieceType]
    pieceOrder = [UnmovedRook, Knight, Bishop, Queen, UnmovedKing, Bishop, Knight, UnmovedRook]

projectMove :: Move -> Board -> S.Seq Square
projectMove (Move (MKingMv KUp) (r, f) piece) board = if f == Eight || fmap color (pieceAt board (r, succ f)) == Just (color piece) then [] else [(r, succ f)]
projectMove _ _ = error "Not yet implemented"
