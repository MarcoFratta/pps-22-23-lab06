package u06lab.solution

import u06lab.solution.ConnectThree.Player.{O, X}
import u06lab.solution.ConnectThree.*

import java.util.OptionalInt
import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}

// Optional!
object ConnectThree:
  val bound = 3

  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(d => d.x == x && d.y == y).map(_.player)

  def firstAvailableRow(board: Board, x: Int)(using size:Int): Option[Int] =
    Option(board.filter(_.x == x).foldLeft(0)((b,_) => b+1)).filter(_ < size)
  def placeAnyDisk(board: Board, player: Player)(using size :Int): Seq[Board] =
    (0 until size) map(x => firstAvailableRow(board, x).fold(board)(board :+ Disk(x,_,player)))

  def computeAnyGame(player: Player, moves: Int)(using f:(Board,Player) => Seq[Board])(using l:Int): LazyList[Game] = moves match
    case 1 => f(List(), player) map (Seq(_)) to LazyList
    case _ => for g <- computeAnyGame(player.other, moves - 1)
                  win = isWin(g.head)
                  b <- if win then LazyList(Seq()) else f(g.head, player) filter (_ != g.head)
                    yield if win then g else g prepended b


  extension[A] (l: List[A])
    @tailrec
    private def forAllCouple(f: (A, A) => Boolean): Boolean = l match
      case Nil | _ :: Nil => false
      case a :: b :: Nil => f(a, b)
      case a :: b :: c :: Nil => f(a, b) && f (b,c)
      case a :: b :: t => if f(a, b) then t forAllCouple f else false

  private def isWin(board: Board)(using l:Int):Boolean =
    def checkPlayerWin(player: Player): Boolean =
      val shift = absDistance(bound,l)
      List((-shift to shift).map(offset => board.filter(d => (d.x + offset) == d.y)),
        (-shift to shift).map(offset => board.filter(d => (l - 1 - d.x + offset) == d.y )),
        (0 until l).map(i => board.filter(d => d.x == i )),
        (0 until l).map(i => board.filter(d => d.y == i )))
        .exists(segment => segment.map(b => b filter(_.player == player)).filter(_.length >= bound)
          .exists(g => g.toList forAllCouple ((a, b) => areAdjacent((a.x, a.y), (b.x, b.y)))))
    checkPlayerWin(X) || checkPlayerWin(O)

  private def absDistance(x:Int, y:Int):Int = Math.abs(x - y)

  def areAdjacent(a: (Int, Int), b: (Int, Int)): Boolean = (a._1,a._2,b._1,b._2) match
    case (x1, y1, x2, y2) if x1 == x2 => absDistance(y1, y2) == 1
    case (x1, y1, x2, y2) if y1 == y2 => absDistance(x1, x2) == 1
    case _ => absDistance(a._1,b._1) + absDistance(a._2,b._2) <= 2

  def printBoards(game: Seq[Board])(using size:Int): Unit =
    for
      y <- (size - 1) to 0 by -1
      board <- game.reverse
      x <- 0 until size
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == size - 1 then
        print(" ")
          if board == game.head then println()


@main def main(args:String*):Unit =
  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")

  given f: ((Board, Player) => Seq[Board]) = placeAnyDisk

  given l: Int = 4 // the length of the square

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 9).foreach { g =>
    println(s"game length ${g.length}")
    printBoards(g)
    println()
  }
// println(computeAnyGame(O,1).last)
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!