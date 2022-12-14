package chessGame.pieces

import chessGame.movement.{Direction, Directions}

case class Bishop(colour: Colour, ordinal: Int) extends ChessPiece(colour, ordinal) {

  override def getMovingDirections: Set[Direction] = Set(Directions.NE, Directions.SE, Directions.SW, Directions.NW)

  override def toString: String = if(isWhite) "B" else "b"

}
