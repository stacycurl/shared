package sjc.shared

import org.junit.Test
import org.scalacheck._
import scalaz._

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
    val stringCallback = Callback.CallbackContravariant.contramap[Int, String](intCallback)(_.toInt)

    stringCallback(Change("123", "456"))
    assertEquals(List(Change(123, 456)), ints.get())
  }
}

object CallbackSpec extends BaseSpec("Callback") {
  import scalaz.scalacheck.ScalazProperties._
  import ChangeSpec._

  implicit val intCallback: Arbitrary[Callback[Int]] = Arbitrary {
    Gen.const(Callback[Int](_ => ()))
  }

  implicit def equalIntCallback: Equal[Callback[Int]] =
    Equal.equalBy[Callback[Int], Change[Int]](_.apply(Change(123, 456)))

  checkAll(contravariant.laws[Callback])
}
