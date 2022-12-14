package chessGame.movement

abstract class VectorContainer

case class SingleVector(direction: Direction, size: Int) extends VectorContainer

case class VectorPair(v1: SingleVector, v2: SingleVector) extends VectorContainer