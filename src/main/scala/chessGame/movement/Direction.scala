package chessGame.movement

import scala.util.{Failure, Success, Try}

sealed trait Direction {
  val point: Point
}

case object Directions {

  def apply(point: Point): Try[Direction] =
    point match {
      case Directions.N.point => Success(Directions.N)
      case Directions.NE.point => Success(Directions.NE)
      case Directions.E.point => Success(Directions.E)
      case Directions.SE.point => Success(Directions.SE)
      case Directions.S.point => Success(Directions.S)
      case Directions.SW.point => Success(Directions.SW)
      case Directions.W.point => Success(Directions.W)
      case Directions.NW.point => Success(Directions.NW)
      case _ => Failure(new IllegalArgumentException("Unable to build a direction from the given point."))
    }

  private def getSingleVector(subtracted: Point, size: Int): Try[VectorContainer] = {
    val subX = subtracted.x
    val subY = subtracted.y
    val subXAbs = subX.abs
    val subYAbs = subY.abs
    val xDivision = if (subX == 0) 0 else subX / subXAbs
    val yDivision = if (subY == 0) 0 else subY / subYAbs
    Directions(Point(xDivision, yDivision)) match {
      case Failure(exception) => Failure(exception)
      case Success(direction) => Success(SingleVector(direction, size.abs))
    }
  }

  private def getVectorPair(subtracted: Point): Try[VectorContainer] = {
    val subX = subtracted.x
    val subY = subtracted.y
    val subXAbs = subX.abs
    val subYAbs = subY.abs
    val xDivision = if (subX == 0) 0 else subX / subXAbs
    val yDivision = if (subY == 0) 0 else subY / subYAbs
    val tryV1: Try[SingleVector] = Directions(Point(0, yDivision)) match {
      case Failure(exception) => Failure(exception)
      case Success(direction) => Success(SingleVector(direction, subYAbs))
    }
    val tryV2: Try[SingleVector] = Directions(Point(xDivision, 0)) match {
      case Failure(exception) => Failure(exception)
      case Success(direction) => Success(SingleVector(direction, subXAbs))
    }
    (tryV1, tryV2) match {
      case (Success(v1), Success(v2)) => Success(VectorPair(v1, v2))
      case (Failure(e1), _) => Failure(e1)
      case (_, Failure(e2)) => Failure(e2)
    }
  }

  def getDirectionVector(from: Point, to: Point): Try[VectorContainer] = {
    val subtracted = from.getSubtraction(to)
    subtracted match {
      case Point(n, 0) => getSingleVector(subtracted, n)
      case Point(0, n) => getSingleVector(subtracted, n)
      case Point(n, _) =>
        if (subtracted.x.abs == subtracted.y.abs)
          getSingleVector(subtracted, n)
        else
          getVectorPair(subtracted)
    }
  }

  case object N extends Direction {
    val point: Point = Point(0, 1)
  } // North

  case object NE extends Direction {
    val point: Point = Point(1, 1)
  } // North-East

  case object E extends Direction {
    val point: Point = Point(1, 0)
  } // East

  case object SE extends Direction {
    val point: Point = Point(1, -1)
  } // South-East

  case object S extends Direction {
    val point: Point = Point(0, -1)
  } // South

  case object SW extends Direction {
    val point: Point = Point(-1, -1)
  } // South-West

  case object W extends Direction {
    val point: Point = Point(-1, 0)
  } // West

  case object NW extends Direction {
    val point: Point = Point(-1, 1)
  } // North-West

  val all: Set[Direction] = Set[Direction](N, NE, E, SE, S, SW, W, NW)
}