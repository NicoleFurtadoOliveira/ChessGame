package chessGame.movement

case class Point(x: Int, y: Int) {

  def getSum(other: Point): Point = Point(x + other.x, y + other.y)

  def getSubtraction(other: Point): Point = Point(other.x - x, other.y - y)

}
