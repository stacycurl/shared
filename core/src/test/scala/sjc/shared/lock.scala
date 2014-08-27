package sjc.shared

import org.junit.Test
import scalaz._

import org.junit.Assert._


class LockTests {
  @Test def tap {
    val events = Shared[List[LockEvent]](Nil)
    val tapped = Unlocked.tap(events += _)

    tapped.withRead(assertEquals(List(EnteringRead(Unlocked)), events.drain()))
    assertEquals(List(LeavingRead(Unlocked)), events.drain())

    tapped.withWrite(assertEquals(List(EnteringWrite(Unlocked)), events.drain()))
    assertEquals(List(LeavingWrite(Unlocked)), events.drain())
  }

  @Test def zip {
    val (left, right) = {
      val (one, two) = (Synchronized("one"), ReadWriteLock())

      if (one.identity < two.identity) (one, two) else (two, one)
    }

    assert(left.identity < right.identity)

    assertEquals(ZippedLock(left, right), left.zip(right))

    val events = Shared[List[LockEvent]](Nil)
    val zipped = ZippedLock(left.tap(events += _), right.tap(events += _))

    zipped.withRead(assertEquals(List(EnteringRead(left), EnteringRead(right)), events.drain()))
    assertEquals(List(LeavingRead(right), LeavingRead(left)), events.drain())

    zipped.withWrite(assertEquals(List(EnteringWrite(left), EnteringWrite(right)), events.drain()))
    assertEquals(List(LeavingWrite(right), LeavingWrite(left)), events.drain())
  }
}
