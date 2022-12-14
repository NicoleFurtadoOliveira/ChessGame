package chessGame.pieces

case class King(colour: Colour) extends ChessPiece(colour, maxSteps = 1) {

  override def toString: String = if(isWhite) "K" else "k"

}
