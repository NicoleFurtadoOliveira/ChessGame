package chessGame.pieces

import chessGame.movement._
import chessGame.{Board, InvalidState, StateName, ValidState}

import scala.util.{Failure, Success, Try}

case class Knight(colour: Colour, ordinal: Int) extends ChessPiece(colour, ordinal, maxSteps = 2) {

  override def getMovements(board: Board, point: Point): Set[Movement] = {

    def clean(toClean: Set[Try[Movement]]): Set[Movement] = {
      toClean.map {
        case Failure(exception) => throw exception
        case Success(move) => move
      }.filter(pointTo => board.isInsideLimits(pointTo.to))
    }

    def getNorthSouthMoves(direction: Direction): Set[Movement] = clean(Set(
      Movement(point, point.getSum(direction.point).getSum(direction.point).getSum(Directions.E.point)),
      Movement(point, point.getSum(direction.point).getSum(direction.point).getSum(Directions.W.point))))

    def getEastWestMoves(direction: Direction): Set[Movement] = clean(Set(
      Movement(point, point.getSum(direction.point).getSum(direction.point).getSum(Directions.N.point)),
      Movement(point, point.getSum(direction.point).getSum(direction.point).getSum(Directions.S.point))))

    getNorthSouthMoves(Directions.N) ++
      getEastWestMoves(Directions.E) ++
      getNorthSouthMoves(Directions.S) ++
      getEastWestMoves(Directions.W)
  }

  private def illegalVectorSizes(sizeV1: Int, sizeV2: Int): Boolean = {
    val (bigger, smaller) = if (sizeV1 > sizeV2) (sizeV1, sizeV2) else (sizeV2, sizeV1)
    bigger != getMaxSteps || smaller != bigger - 1
  }

  private def evaluateStateAfterAux(movement: Movement): StateName =
    movement.vectorContainer match {
      case SingleVector(_, _) => InvalidState.illegalMove
      case VectorPair(v1, v2) =>
        if ((v1.direction == v2.direction) || illegalVectorSizes(v1.size, v2.size))
          InvalidState.illegalMove
        else
          ValidState.legalMove
    }

  override def evaluateStateAfter(movement: Movement, board: Board): StateName =
    super.evaluateStateAfter(movement, board) match {
      case ValidState(_) => evaluateStateAfterAux(movement)
      case InvalidState(state) => state match {
        case InvalidState.IllegalTramplePieces => evaluateStateAfterAux(movement)
        case _ => InvalidState.illegalMove
      }
    }

  override def getMovingDirections: Set[Direction] = Set(Directions.N, Directions.E, Directions.S, Directions.W)

  override def toString: String = if (isWhite) "N" else "n"

}
