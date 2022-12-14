package chessGame

import chessGame.Constants._
import chessGame.ValidState.PawnFirstMove
import chessGame.movement.{Movement, Point}
import chessGame.pieces._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Board(size: Int) {

  private val pointsToPiece: mutable.HashMap[Point, ChessPiece] = new mutable.HashMap()
  private val pieceToPoints: mutable.HashMap[ChessPiece, Point] = new mutable.HashMap()
  private val capturedWhitePieces: mutable.ArrayBuffer[ChessPiece] = new ArrayBuffer[ChessPiece]()
  private val capturedBlackPieces: mutable.ArrayBuffer[ChessPiece] = new ArrayBuffer[ChessPiece]()
  private val pawnsThatMadeFirstMove: mutable.ArrayBuffer[ChessPiece] = new ArrayBuffer[ChessPiece]()

  def generateMovesOfEnemies(currentColour: Colour): Set[Movement] = {
    val moves: mutable.Set[Movement] = mutable.Set.empty
    for ((point, piece) <- pointsToPiece.toMap)
      if (currentColour != piece.getColour)
        moves ++= piece.getMovements(this, point)
    moves.toSet
  }

  private def removePiece(chessPiece: ChessPiece): Unit =
    pieceToPoints.get(chessPiece).collect { pointsToDelete =>
      pointsToPiece -= pointsToDelete
      pieceToPoints -= chessPiece
    }

  private def removePieceFromCaptured(chessPiece: ChessPiece): Unit = {
    if (chessPiece.isWhite)
      capturedWhitePieces -= chessPiece
    else
      capturedBlackPieces -= chessPiece
  }

  def capturePiece(chessPiece: ChessPiece): Unit = {
    removePiece(chessPiece)
    chessPiece.getColour match {
      case Colour.Black => capturedBlackPieces += chessPiece
      case Colour.White => capturedWhitePieces += chessPiece
    }
  }

  def init(): Unit = {
    place(Rook(Colour.Black, 1), Point(FirstIndex, FirstIndex), None)
    place(Knight(Colour.Black, 1), Point(SecondIndex, FirstIndex), None)
    place(Bishop(Colour.Black, 1), Point(ThirdIndex, FirstIndex), None)
    place(Queen(Colour.Black), Point(FourthIndex, FirstIndex), None)
    place(King(Colour.Black), Point(FifthIndex, FirstIndex), None)
    place(Bishop(Colour.Black, 2), Point(SixthIndex, FirstIndex), None)
    place(Knight(Colour.Black, 2), Point(SeventhIndex, FirstIndex), None)
    place(Rook(Colour.Black, 2), Point(EighthIndex, FirstIndex), None)
    for (x <- FirstIndex until size)
      place(Pawn(Colour.Black, x + 1), Point(x, SecondIndex), None)
    place(Rook(Colour.White, 1), Point(FirstIndex, EighthIndex), None)
    place(Knight(Colour.White, 1), Point(SecondIndex, EighthIndex), None)
    place(Bishop(Colour.White, 1), Point(ThirdIndex, EighthIndex), None)
    place(Queen(Colour.White), Point(FourthIndex, EighthIndex), None)
    place(King(Colour.White), Point(FifthIndex, EighthIndex), None)
    place(Bishop(Colour.White, 2), Point(SixthIndex, EighthIndex), None)
    place(Knight(Colour.White, 2), Point(SeventhIndex, EighthIndex), None)
    place(Rook(Colour.White, 2), Point(EighthIndex, EighthIndex), None)
    for (x <- FirstIndex until size)
      place(Pawn(Colour.White, x + 1), Point(x, SeventhIndex), None)
  }

  def place(chessPiece: ChessPiece, points: Point, state: Option[StateName]): Unit = {
    removePiece(chessPiece)
    pointsToPiece += points -> chessPiece
    pieceToPoints += chessPiece -> points
    chessPiece match {
      case pawn@Pawn(_, _) =>
        state.foreach {
          case ValidState(PawnFirstMove) =>
            if (!hasMadeFirstMove(pawn))
              pawnsThatMadeFirstMove += pawn
          case _ => ;
        }
      case _ => ;
    }
  }

  def undo(movement: Movement, piece: ChessPiece, enemyPiece: Option[ChessPiece], state: Option[StateName]): Unit = {
    place(piece, movement.from, state)
    piece match {
      case pawn@Pawn(_, _) =>
        state.foreach {
          case ValidState(PawnFirstMove) =>
            if (hasMadeFirstMove(pawn))
              pawnsThatMadeFirstMove -= pawn
          case _ => ;
        }
      case _ => ;
    }
    enemyPiece.foreach { enemyPiece =>
      place(enemyPiece, movement.to, state)
      removePieceFromCaptured(enemyPiece)
    }
  }

  def isInsideLimits(point: Point): Boolean = point.x >= 0 && point.x < size && point.y >= 0 && point.y < size

  def hasMadeFirstMove(piece: Pawn): Boolean = pawnsThatMadeFirstMove.contains(piece)

  def maybeGetPiece(points: Point): Option[ChessPiece] = pointsToPiece.get(points)

  def maybeGetOpponentPieceInPosition(position: Point, piece: ChessPiece): Option[ChessPiece] =
    maybeGetPiece(position).filter(isEnemy(_, piece))

  def isEnemy(p1: ChessPiece, p2: ChessPiece): Boolean = !p1.getColour.equals(p2.getColour)

  def toString(points: Point): String =
    maybeGetPiece(points) match {
      case Some(value) => value.toString
      case None => " "
    }

  def printCapturedPieces(): Unit = {
    var stringBlack = ""
    for (piece <- capturedBlackPieces) {
      stringBlack += s"$piece\n"
    }
    var stringWhite = ""
    for (piece <- capturedWhitePieces) {
      stringWhite += s"$piece\n"
    }
    if (stringWhite.nonEmpty)
      print(s"Captured white pieces:\n$stringWhite")
    else
      println("No white pieces where captured.")
    if (stringBlack.nonEmpty)
      print(s"Captured black pieces:\n$stringBlack")
    else
      println("No black pieces where captured.")
  }

}