package u06lab.solution

import org.junit.Assert.assertEquals
import org.junit.Test

class SolitaireTest {
  import Solitaire.*

  given moves: Set[(Int,Int)] = Set((-2, -2), (2, 2), (-2, 2), (2, -2),
    (3, 0), (0, 3), (-3, 0), (0, -3))
  @Test def testPossibleMoves(): Unit =
    assertEquals(Set((1, 1), (5, 5), (1, 5), (5, 1),
      (3, 0), (3, 6), (0, 3), (6, 3)), getMoves((3,3)))
}
