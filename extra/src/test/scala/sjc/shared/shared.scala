package sjc.shared

import org.junit.Test
import org.scalacheck._
import scalaz._

import org.junit.Assert._
import scalaz.scalacheck.ScalazProperties._
import sjc.shared.instances.shared._
import sjc.shared.Reader._


class SharedTests {
  @Test def canShow {
    implicit val showInt: Show[Int] = Show.shows[Int](i => s"Int: $i")

    assertEquals("Int: 123", sharedShow[Int].shows(Shared(123)))
  }

  @Test def canCreateLens {
    val tuple  = Shared(("one", 1))
    val string = tuple.lens(first)
    val int    = tuple.lens(second)

    assertEquals("one", string.get())
    assertEquals(1,     int.get())

    assertEquals(Change("one", "one >> two"), string.modify(_ ++ " >> two"))
    assertEquals(Change(1, 2),                int.modify(_ + 1))

    assertEquals("one >> two", string.get())
    assertEquals(2,            int.get())

    assertEquals(("one >> two", 2), tuple.get())

    assertSame(tuple.lock, string.lock)
    assertSame(tuple.lock, int.lock)
  }

  @Test def canUnzip {
    val tuple = Shared(("one", 1))
    val (string, int) = SharedInstance.unzip(tuple)

    assertEquals("one", string.get())
    assertEquals(1, int.get())

    assertEquals(Change("one", "two"), string.modify(_ => "two"))

    assertEquals("two", string.get())
    assertEquals(("two", 1), tuple.get())
  }

  @Test def sharedSemigroup {
    val maxInt = Shared[Int](1)
    val changes = maxInt.changes()

    implicit val maxIntSemigroup = Semigroup.instance[Int] {
      case (l, r) => math.max(l, r)
    }

    List(2, 3, 1, 5, 6).foreach(i => maxInt.append(i))

    assertEquals(List(1, 2, 3, 3, 5, 6), changes.values())
  }

  @Test def sharedMonoid {
    implicit val minIntMonoid = Monoid.instance[Int]((l, r) => math.min(l, r), Int.MaxValue)
    val int = Shared(1)

    int.clear()

    assertEquals(Int.MaxValue, int.get())
  }

  @Test def lensSharedCanNotifyOnChange {
    val tuple  = Shared(("one", 1))
    val string = tuple.lens(first)

    val tupleChanges  = tuple.changes()
    val stringChanges = string.changes()

    assertEquals(Nil, stringChanges.get())

    tuple.value = ("two", 2)

    assertEquals(Change.many(("one", 1), ("two", 2)), tupleChanges.get())
    assertEquals(Change.many("one", "two"),           stringChanges.get())

    string.value = "three"

    assertEquals(Change.many(("one", 1), ("two", 2), ("three", 2)), tupleChanges.get())
    assertEquals(Change.many("one", "two", "three"),                stringChanges.get())
  }

  private val first  = Lens.firstLens[String, Int]
  private val second = Lens.secondLens[String, Int]
}

object SharedSpec extends BaseSpec("Shared") {
  implicit def arbShared[A: Arbitrary]: Arbitrary[Shared[A]] = Arbitrary {
    for (a <- arbitrary[A]) yield Shared[A](a)
  }

  checkAll(invariantFunctor.laws[Shared])
}
