package stacycurl.scala.shared

import org.junit.Test

import org.junit.Assert._
import scalaz._


class ChangeTests {
  @Test def calcPerformsCalculateOnOldAndModifiedValue {
    assertEquals(List("initial", "initial >> modified"),
      Change("initial", "initial >> modified").calc {
        case (initial, modified) => List(initial, modified)
      }
    )
  }

  @Test def canMapOver {
    assertEquals(Change("1", "2"), Change(1, 2).map(_.toString))
  }

  @Test def canZip {
    assertEquals(Change(("one", 1), ("two", 2)), Change("one", "two").zip(Change(1, 2)))
  }

  @Test def canJoin {
    assertEquals(Change(1, 3), Change(1, 2).join(Change(2, 3)))
  }

  @Test def canGetDeltaOfIntChange {
    assertEquals(3, Change(1, 4).delta)
  }

  @Test def canFilter {
    assertEquals(Change(1, 1), Change(1, 2).filter(_.delta > 1))
    assertEquals(Change(1, 3), Change(1, 3).filter(_.delta > 1))
  }
}
