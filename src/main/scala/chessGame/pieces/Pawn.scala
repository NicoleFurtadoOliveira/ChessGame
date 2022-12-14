package chessGame.pieces

import chessGame.movement._
import chessGame.{Board, InvalidState, StateName, ValidState}

case class Pawn(colour: Colour, ordinal: Int) extends ChessPiece(colour, ordinal, maxSteps = 2) {

  override def evaluateStateAfter(movement: Movement, board: Board): StateName = {
    super.evaluateStateAfter(movement, board) match {
      case ValidState(_) => movement.vectorContainer match {
        case SingleVector(direction, size) =>
          //if diagonal move without an enemy or the step is bigger than 1
          if ((direction != Directions.S && direction != Directions.N) &&
            (board.maybeGetOpponentPieceInPosition(movement.to, this).isEmpty || size > 1))
            InvalidState.illegalMove
          else if (board.hasMadeFirstMove(this) && size > 1)
            InvalidState.illegalMove
          else {
            ValidState.pawnFirstMove
          }
        case VectorPair(_, _) => InvalidState.illegalMove
      }
      case InvalidState(state) => InvalidState(state)
    }
  }

  override def getMovingDirections: Set[Direction] =
    if (isWhite)
      Set(Directions.S, Directions.SW, Directions.SE)
    else
      Set(Directions.N, Directions.NW, Directions.NE)

  override def toString: String = if (isWhite) "P" else "p"

}
