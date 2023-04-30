package u06lab.solution

import u06lab.solution.Solitaire.Board

object Solitaire:

  private type Position = (Int, Int)
  private type Solution = Seq[Position]
  private type Board = Seq[Int]
  private type SolutionFactory = Solution => Iterable[Solution]
  private val verticalStep: Int = 3;
  private val diagonalStep: Int = 2
  private val moves: Set[Position] = Set((-diagonalStep, -diagonalStep), (diagonalStep, diagonalStep),
    (-diagonalStep, diagonalStep), (diagonalStep, -diagonalStep),
    (verticalStep, 0), (0, verticalStep),
    (-verticalStep, 0), (0, -verticalStep))

  given SolutionFactory = x => LazyList(x).view

  def render(solution: Solution, width: Int, height: Int): Unit =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    println(rows.mkString("\n") + "\n")

  def main(args: Array[String]): Unit = {
    val w = 7
    val h = 5
    println(s"Computing solutions for $w x $h grid...")
    var solution: Int = 0
    placeMarks(w, h) foreach (x =>
      render(x, w, h)
      solution = solution + 1)
    println(s"Number of solutions:" + solution)
  }

  private def getIndex(w: Int)(p: Position) = (p._2 * w) + p._1
  def getMoves(k: Position): Iterable[Position] = moves.map(x => (k._1 + x._1, k._2 + x._2))

  private def solutions(b: Board)(k: Position)(i: Int)(f: Position => Int)(max: Int)
                       (using factory: SolutionFactory): Iterable[Solution] = i match
    case _ if b(f(k)) != 0 => Iterable.empty
    case `max` => factory(List(k))
    case _ => getMoves(k) flatMap (p => solutions(b updated(f(k), i))(p)(i + 1)(f)(max)
      .flatMap(s => factory(s appended k)))

  private def placeMarks(w: Int, h: Int): Iterable[Solution] =
    val l = Math.max(verticalStep, diagonalStep)
    val W = w + (l * 2)
    val H = h + (l * 2)
    var x = List.iterate(-1, W * H)(a => a)
    for i <- l until l + h do for j <- l until l + w do x = x updated(getIndex(W)((j, i)), 0)
    solutions(x)(W / 2, H / 2)(1)(getIndex(W))(w * h) map (s => s map (p => (p._1 - l, p._2 - l)))


