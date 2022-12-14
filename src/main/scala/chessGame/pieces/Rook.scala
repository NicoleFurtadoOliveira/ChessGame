package chessGame.pieces

import chessGame.movement.{Direction, Directions}

case class Rook(colour: Colour, ordinal: Int) extends ChessPiece(colour, ordinal) {

  override def getMovingDirections: Set[Direction] = Set(Directions.N, Directions.E, Directions.S, Directions.W)

  override def toString: String = if(isWhite) "R" else "r"

}
