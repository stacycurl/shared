package sjc.shared

import org.junit.Test

import org.junit.Assert._


class CallbackTests {
  @Test def canGuard {
    val intChanges = Shared[List[Change[Int]]](Nil)

    val allInts = Callback[Int](intChanges += _)
    val guard = Shared(false)

    val guarded = allInts.guard(guard)

    guarded(Change(1, 2))
    assertEquals(Nil, intChanges.get())

    guard.value = true

    guarded(Change(1, 2))
    assertEquals(List(Change(1, 2)), intChanges.get())
  }

  @Test def canFilter {
    val intChanges = Shared[List[Change[Int]]](Nil)

    val allInts = Callback[Int](intChanges += _)
    val filtered = allInts.filter(_.delta > 1)

    allInts(Change(1, 2))
    assertEquals(List(Change(1, 2)), intChanges.get())

    filtered(Change(1, 2))
    assertEquals(List(Change(1, 2)), intChanges.get())

    filtered(Change(1, 3))
    assertEquals(List(Change(1, 2), Change(1, 3)), intChanges.get())
  }

  @Test def canContramap {
    val ints = Shared[List[Change[Int]]](Nil)
    val intCallback = Callback[Int](ints += _)
    val stringCallback = intCallback.contramap[String](_.toInt)

    stringCallback(Change("123", "456"))
    assertEquals(List(Change(123, 456)), ints.get())
  }
}
