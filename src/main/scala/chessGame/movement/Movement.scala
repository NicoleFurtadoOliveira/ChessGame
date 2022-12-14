package chessGame.movement

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class Movement(from: Point, vectorContainer: VectorContainer, to: Point) {

  override def toString: String = {
    s"Movement from (${from.x}, ${from.y}) to (${to.x}, ${to.y})"
  }

  def gatherPoints(): Seq[Point] = {
    var points: mutable.Seq[Point] = mutable.Seq.empty
    vectorContainer match {
      case SingleVector(direction, size) =>
        for (_ <- 1 until size)
          points :+= from.getSum(direction.point)
        points.toSeq
      case VectorPair(v1, v2) =>
        for (_ <- 1 until v1.size)
          points :+= from.getSum(v1.direction.point)
        if (points.nonEmpty) {
          var lastPoint = points.last
          for (_ <- 1 until v2.size) {
            lastPoint = lastPoint.getSum(v2.direction.point)
            points :+= lastPoint
          }
        }
        points.toSeq
    }
    points.toSeq
  }

}

case object Movement {

  def apply(from: Point, to: Point): Try[Movement] =
    Directions.getDirectionVector(from, to) match {
      case Failure(exception) => Failure(exception)
      case Success(vectorContainer) => Success(Movement(from, vectorContainer, to))
    }

}
