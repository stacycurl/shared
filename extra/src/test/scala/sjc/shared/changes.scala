package sjc.shared

import org.junit.Test
import org.scalacheck._
import scalaz.Equal

import org.junit.Assert._
import scalaz.scalacheck.ScalazProperties._
import sjc.shared.ChangeSpec._
import sjc.shared.Reader._
import sjc.shared.instances.changes._
import sjc.shared.instances.change._


class ChangesTests {
  @Test def canUnzip {
    val (left, right) = ChangesInstance.unzip(Changes.many((1, "one"), (2, "two")))

    assertEquals(List(Change(1, 2)), left.get())
    assertEquals(List(Change("one", "two")), right.get())
  }

  @Test def point {
    assertEquals(List(Change.point(1)), ChangesInstance.point(1).get())
  }
}

object ChangesSpec extends BaseSpec("Changes") {
  implicit def changes[A](implicit change: Arbitrary[Change[A]]): Arbitrary[Changes[A]] = Arbitrary {
    for (change <- change.arbitrary) yield Changes[A](change)
  }

  implicit val intChanges: Arbitrary[Changes[Int]] = Arbitrary {
    for (change <- arbitrary[Change[Int]]) yield Changes[Int](change)
  }

  checkAll(bind.laws[Changes])
  checkAll(equal.laws[Changes[Int]])
  checkAll(functor.laws[Changes])
  checkAll(zip.laws[Changes])
}
