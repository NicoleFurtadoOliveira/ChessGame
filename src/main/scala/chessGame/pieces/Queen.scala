package chessGame.pieces

case class Queen(colour: Colour) extends ChessPiece(colour) {

  override def toString: String = if(isWhite) "Q" else "q"

}
