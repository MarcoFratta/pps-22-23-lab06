package u06lab.solution

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

class ConnectThreeTest:

  import ConnectThree.*

  @Test def testAdjacent():Unit =
    assertTrue(areAdjacent((0,0),(0,1)))
    assertFalse(areAdjacent((0,0),(0,0)))
    assertTrue(areAdjacent((0,0),(1,1)))
    assertFalse(areAdjacent((0,0),(2,2)))
    assertTrue(areAdjacent((0,0), (-1,-1)))
    assertFalse(areAdjacent((0,0), (1,2)))




