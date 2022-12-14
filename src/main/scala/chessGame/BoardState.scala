package chessGame

sealed abstract class State

sealed abstract class StateName(state: State) {
  def getState: State = state
}

case class ValidState(state: State) extends StateName(state)

case class InvalidState(state: State) extends StateName(state)

object ValidState {

  def moveToEmptySpot: StateName = ValidState(MoveToEmptySpot)

  def captureEnemyPiece: StateName = ValidState(CaptureEnemyPiece)

  def gameOver: StateName = ValidState(GameOver)

  def check: StateName = ValidState(Check)

  def legalMove: StateName = ValidState(LegalMove)

  def pawnFirstMove: StateName = ValidState(PawnFirstMove)

  case object MoveToEmptySpot extends State

  case object CaptureEnemyPiece extends State

  case object GameOver extends State

  case object Check extends State

  case object LegalMove extends State

  case object PawnFirstMove extends State

}

object InvalidState {

  def noPieceOnStartPosition: StateName = InvalidState(NoPieceOnStartPosition)

  def illegalCaptureSameColorPiece: StateName = InvalidState(IllegalCaptureSameColorPiece)

  def illegalMove: StateName = InvalidState(IllegalMove)

  def illegalMoveKeepsCheck: StateName = InvalidState(IllegalMoveKeepsCheck)

  def illegalTramplePieces: StateName = InvalidState(IllegalTramplePieces)

  def illegalInCheck: StateName = InvalidState(IllegalInCheck)

  def outOfBoardLimits: StateName = InvalidState(OutOfBoardLimits)

  def noMoreMovesAvailable: StateName = InvalidState(NoMoreMovesAvailable)

  def illegalMoveWrongPlayer: StateName = InvalidState(IllegalMoveWrongPlayer)

  case object NoPieceOnStartPosition extends State

  case object IllegalCaptureSameColorPiece extends State

  case object IllegalMove extends State

  case object IllegalMoveKeepsCheck extends State

  case object IllegalTramplePieces extends State

  case object IllegalInCheck extends State

  case object OutOfBoardLimits extends State

  case object NoMoreMovesAvailable extends State

  case object IllegalMoveWrongPlayer extends State

}



