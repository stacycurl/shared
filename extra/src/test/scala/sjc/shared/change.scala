package sjc.shared

import org.junit.Test
import org.scalacheck._
import scalaz._

import org.junit.Assert._
import scalaz.scalacheck.ScalazProperties._
import sjc.shared.instances.change._


class ChangeTests {
  @Test def canUnzip {
    assertEquals((Change("one", "two"), Change(1, 2)),
      ChangeInstance.unzip(Change(("one", 1), ("two", 2))))
  }
}

object ChangeSpec extends BaseSpec("Change") {
  implicit def change[A](implicit arb: Arbitrary[A]): Arbitrary[Change[A]] = Arbitrary {
    for { before <- arb.arbitrary; after <- arb.arbitrary } yield Change[A](before, after)
  }

  checkAll(applicative.laws[Change])
  checkAll(equal.laws[Change[Int]])
  checkAll(zip.laws[Change])
}
