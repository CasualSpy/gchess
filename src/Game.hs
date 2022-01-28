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

attackedSquares :: Move -> Board -> S.Seq Square
attackedSquares (Move (MKingMv dir) sq piece) board = movedKingMove dir sq (color piece) board
attackedSquares _ _ = error "Not yet implemented"

movedKingMove :: MovedKingMove -> Square -> Color -> Board -> S.Seq Square
movedKingMove dir (f, r) c b = if Just r == maxRank || Just f == maxFile || fmap color (pieceAt b attackedSquare) == Just c then [] else [attackedSquare]
  where
    attackedSquare :: Square
    attackedSquare = case dir of
      KUpLeft -> (pred f, succ r)
      KUp -> (f, succ r)
      KUpRight -> (succ f, succ r)
      KRight -> (succ f, r)
      KDownRight -> (succ f, pred r)
      KDown -> (f, pred r)
      KDownLeft -> (pred f, pred r)
      KLeft -> (pred f, r)
    maxRank :: Maybe Rank
    maxRank = case dir of
      KUpLeft -> Just Eight
      KUp -> Just Eight
      KUpRight -> Just Eight
      KDownLeft -> Just One
      KDown -> Just One
      KDownRight -> Just One
      _ -> Nothing
    maxFile :: Maybe File
    maxFile = case dir of
      KUpLeft -> Just A
      KLeft -> Just A
      KDownLeft -> Just A
      KUpRight -> Just H
      KRight -> Just H
      KDownRight -> Just H
      _ -> Nothing
