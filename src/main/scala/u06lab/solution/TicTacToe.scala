package u06lab.solution

import u06lab.solution.ConnectThree
import u06lab.solution.ConnectThree.*

object TicTacToe extends App:

  import ConnectThree.*


  def s(board: Board, player: Player)(using size:Int): Seq[Board] =
    (for i <- 0 until size
        j <- 0 until size yield find(board,i,j).fold(board.appended(Disk(i,j,player)))(_ => Seq.empty))
      .filter(_.nonEmpty)

  given size:Int = 3
  given f: ((Board, Player) => Seq[Board]) = s
  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(Player.X, 6).foreach { g =>
    println(s"game length ${g.length}")
    printBoards(g)
    println()
  }


