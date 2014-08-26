package sjc.shared

import org.junit.Test
import org.scalacheck._

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
    assertEquals(new Unchanged(2), new Unchanged(1).map(_ * 2))
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

  @Test def canFold {
    assertEquals(3, Change(1, 2).fold(_ => 10)(ci => (ci.before + ci.after)))
    assertEquals(3, new Unchanged(3).fold(ui => ui.value)(ci => 10))
  }

  @Test def canRevert {
    val int = Shared(1)

    val change = int.modify(_ + 1)
    assertEquals(2, int.get())

    assertEquals(Change(2, 1), change.revert(int))
    assertEquals(1, int.get())

    int.value = 10

    // The revert of a change isn't simply its inverse, as subsequent changes may have happened
    assertEquals(Change(10, 1), change.revert(int))
    assertEquals(1, int.get())

    // Reverting Unchanged does nothing
    val unchanged = int.alter(i => new Unchanged(i))
    assertEquals(unchanged, unchanged.revert(int))
    assertEquals(1, int.get())

    // Even if there's been a subsequent change
    int.value = 100
    assertEquals(unchanged, unchanged.revert(int))
    assertEquals(100, int.get())
  }
}

object ChangeSpec extends BaseSpec("Change") {
  import scalaz.scalacheck.ScalazProperties._

  implicit def change[A](implicit arb: Arbitrary[A]): Arbitrary[Change[A]] = Arbitrary {
    for { before <- arb.arbitrary; after <- arb.arbitrary } yield Change[A](before, after)
  }

  checkAll(applicative.laws[Change])
  checkAll(equal.laws[Change[Int]])
  checkAll(zip.laws[Change])
}
