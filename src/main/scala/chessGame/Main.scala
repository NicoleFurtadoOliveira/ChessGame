package chessGame

import chessGame.InvalidState.NoMoreMovesAvailable
import chessGame.movement.{Movement, Point}
import chessGame.pieces.Colour

import scala.io.Source
import scala.util.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
    val game = new ChessGame
    println("Game started: ")
    game.init()

    val movementsFileName1 = "C:\\Users\\anali\\Desktop\\ChessGame\\ChessGame\\src\\main\\scala\\chessGame\\movesFiles\\sample-moves.txt"
    val movementsFileName2 = "C:\\Users\\anali\\Desktop\\ChessGame\\ChessGame\\src\\main\\scala\\chessGame\\movesFiles\\sample-moves-invalid.txt"
    val movementsFileName3 = "C:\\Users\\anali\\Desktop\\ChessGame\\ChessGame\\src\\main\\scala\\chessGame\\movesFiles\\checkmate.txt"
    val movementsFileName4 = "C:\\Users\\anali\\Desktop\\ChessGame\\ChessGame\\src\\main\\scala\\chessGame\\movesFiles\\checkmate-more-moves.txt"
    val movementsFileName5 = "C:\\Users\\anali\\Desktop\\ChessGame\\ChessGame\\src\\main\\scala\\chessGame\\movesFiles\\more-moves.txt"

    try {
      var currentColour: Colour = Colour.White
      val source = Source.fromFile(movementsFileName3)
      for (line <- source.getLines) {
        val move = Array(line.charAt(0) - 97, 56 - line.charAt(1), line.charAt(2) - 97, 56 - line.charAt(3))
        Movement(Point(move(0), move(1)), Point(move(2), move(3))) match {
          case Failure(exception) => throw exception
          case Success(movement) => game.process(movement, currentColour) match {
            case ValidState(_) => currentColour = Colour.other(currentColour)
            case InvalidState(_) => ;
          }
        }
      }
      source.close
      game.processState(NoMoreMovesAvailable, None, currentColour)
    } catch {
      case e: Throwable => throw e
    }
  }
}
