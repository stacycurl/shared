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
}