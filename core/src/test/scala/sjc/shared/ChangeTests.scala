package sjc.shared

import org.junit.Test

import org.junit.Assert._


class ChangeTests {
  @Test def testToString(): Unit = {
    assertEquals("Change(1,2)", Change(1, 2).toString)
    assertEquals("Unchanged(1)", new Unchanged(1).toString)
  }

  @Test def calcPerformsCalculateOnOldAndModifiedValue(): Unit = {
    assertEquals(List("initial", "initial >> modified"),
      Change("initial", "initial >> modified").calc {
        case (initial, modified) => List(initial, modified)
      }
    )
  }

  @Test def canMapOver(): Unit = {
    assertEquals(Change("1", "2"), Change(1, 2).map(_.toString))
    assertEquals(new Unchanged(2), new Unchanged(1).map(_ * 2))
  }

  @Test def canZip(): Unit = {
    assertEquals(Change(("one", 1), ("two", 2)), Change("one", "two").zip(Change(1, 2)))
  }

  @Test def canJoin(): Unit = {
    assertEquals(Change(1, 3), Change(1, 2).join(Change(2, 3)))
  }

  @Test def canGetDeltaOfIntChange(): Unit = {
    assertEquals(3, Change(1, 4).delta)
  }

  @Test def canFilter(): Unit = {
    assertEquals(Change(1, 1), Change(1, 2).filter(_.delta > 1))
    assertEquals(Change(1, 3), Change(1, 3).filter(_.delta > 1))
  }

  @Test def canFold(): Unit = {
    assertEquals(3, Change(1, 2).fold(_ => 10)(ci => ci.before + ci.after))
    assertEquals(3, new Unchanged(3).fold(ui => ui.value)(ci => 10))
  }

  @Test def canRevert(): Unit = {
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

  @Test def point(): Unit = {
    assertEquals(new Unchanged(1), Change.point(1))
  }

  @Test def tuple(): Unit = {
    assertEquals((1, 2), Change(1, 2).tuple)
    assertEquals((1, 1), new Unchanged(1).tuple)
  }
}
