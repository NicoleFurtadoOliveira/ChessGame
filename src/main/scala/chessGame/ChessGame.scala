package chessGame

import chessGame.Constants._
import chessGame.ValidState.GameOver
import chessGame.movement.{Movement, Point}
import chessGame.pieces._

case object Constants {
  val BoardSize = 8
  val FirstIndex = 0
  val SecondIndex = 1
  val ThirdIndex = 2
  val FourthIndex = 3
  val FifthIndex = 4
  val SixthIndex = 5
  val SeventhIndex = 6
  val EighthIndex = 7
}

class ChessGame {

  private val board: Board = new Board(BoardSize)

  private def kingInCheck(currentColour: Colour): (Boolean, Option[StateName]) = {
    val found = false
    for (movement <- board.generateMovesOfEnemies(currentColour)) {
      val state = process(movement, Colour.other(currentColour), simulation = true)
      state match {
        case ValidState(GameOver) =>
          return (true, Some(state))
        case _ => ;
      }
    }
    (found, None)
  }


  private def dealWithMaybeEnemy(movement: Movement, piece: ChessPiece, enemyPiece: Option[ChessPiece]): StateName =
    enemyPiece.fold {
      board.place(piece, movement.to, None)
      ValidState.moveToEmptySpot
    } {
      enemy =>
        if (!board.isEnemy(piece, enemy))
          InvalidState.illegalCaptureSameColorPiece
        else
          enemy match {
            case King(_) =>
              ValidState.gameOver
            case _ =>
              board.capturePiece(enemy)
              board.place(piece, movement.to, None)
              ValidState.captureEnemyPiece
          }
    }


  private def processCheck(movement: Movement, piece: ChessPiece, currentColour: Colour, enemyPiece: Option[ChessPiece]): Boolean = {
    board.place(piece, movement.to, None)
    val (isKingInCheck, stateType): (Boolean, Option[StateName]) = kingInCheck(currentColour)
    board.undo(movement, piece, enemyPiece, stateType)
    isKingInCheck
  }

  private def moveIfNotOnCheck(movement: Movement, piece: ChessPiece, currentColour: Colour, maybeEnemy: Option[ChessPiece], simulation: Boolean): StateName = {
    if (simulation)
      maybeEnemy match {
        case Some(enemy@King(_)) if board.isEnemy(enemy, piece) => ValidState.gameOver
        case _ => InvalidState.illegalMove
      }
    else if (processCheck(movement, piece, currentColour, maybeEnemy))
      InvalidState.illegalInCheck
    else
      dealWithMaybeEnemy(movement, piece, maybeEnemy)
  }

  def process(movement: Movement, currentColour: Colour, simulation: Boolean = false): StateName = {
    val stateName: StateName = board.maybeGetPiece(movement.from) match {
      case Some(piece) =>
        if (piece.getColour != currentColour && !simulation)
          InvalidState.illegalMoveWrongPlayer
        else
          piece.evaluateStateAfter(movement, board) match {
            case ValidState(_) => moveIfNotOnCheck(movement, piece, currentColour, board.maybeGetPiece(movement.to), simulation)
            case InvalidState(state) => InvalidState(state)
          }
      case None => InvalidState.noPieceOnStartPosition
    }
    if (!simulation) {
      processState(stateName.getState, Some(movement), currentColour)
      val (isKingInCheck, _): (Boolean, Option[StateName]) = kingInCheck(Colour.other(currentColour))
      if (isKingInCheck)
        processState(ValidState.Check, Some(movement), currentColour)
    }
    stateName
  }

  def init(): Unit = {
    board.init()
    render()
  }

  private def render(): Unit = {
    val filesRow = "      0     1     2     3     4     5     6     7"
    val separator = "   +-----+-----+-----+-----+-----+-----+-----+-----+"
    println(s"$filesRow\n$separator")
    for (y <- FirstIndex until BoardSize) {
      print(y)
      for (x <- FirstIndex until BoardSize)
        print(s"  |  ${
          board.toString(Point(x, y))
        }")
      println(s"  |\n$separator")
    }
  }

  private def printState(state: State, maybeMovement: Option[Movement], currentColour: Colour): Unit =
    maybeMovement.foreach(movement => println(s"$currentColour playing; $movement has state: $state"))

  def processState(state: State, maybeMovement: Option[Movement], currentColour: Colour): Unit = {
    state match {
      case ValidState.MoveToEmptySpot =>
        printState(state, maybeMovement, currentColour)
        render()
      case ValidState.CaptureEnemyPiece =>
        printState(state, maybeMovement, currentColour)
        render()
      case ValidState.GameOver =>
        printState(state, maybeMovement, currentColour)
        render()
      case ValidState.Check =>
        println(s"$currentColour has check on ${Colour.other(currentColour)}.")
      case ValidState.LegalMove =>
        printState(state, maybeMovement, currentColour)
        render()
      case ValidState.PawnFirstMove =>
        printState(state, maybeMovement, currentColour)
        render()
      case InvalidState.NoPieceOnStartPosition => printState(state, maybeMovement, currentColour)
      case InvalidState.IllegalCaptureSameColorPiece => printState(state, maybeMovement, currentColour)
      case InvalidState.IllegalMove => printState(state, maybeMovement, currentColour)
      case InvalidState.IllegalMoveKeepsCheck => printState(state, maybeMovement, currentColour)
      case InvalidState.IllegalTramplePieces => printState(state, maybeMovement, currentColour)
      case InvalidState.IllegalInCheck => printState(state, maybeMovement, currentColour)
      case InvalidState.OutOfBoardLimits => printState(state, maybeMovement, currentColour)
      case InvalidState.IllegalMoveWrongPlayer => printState(state, maybeMovement, currentColour)
      case InvalidState.NoMoreMovesAvailable =>
        printState(state, maybeMovement, currentColour)
        println("No more moves available, game is over.")
        endGame()
    }
  }

  def endGame(): Unit =
    board.printCapturedPieces()

}
