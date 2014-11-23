package sjc.shared

import org.junit.Test
import org.scalacheck._
import scalaz._

import org.junit.Assert._
import scalaz.scalacheck.ScalazProperties._
import sjc.shared.instances.modify._


class ModifyTests {
  @Test def modifyCanBeAppliedToAPartOfAnotherShared(): Unit = {
    assertEquals(Change(("one", 1), ("one", 2)),
      Shared(("one", 1)).modify(Modify[Int](_ + 1).lens(second)))
  }

  private val second = Lens.secondLens[String, Int]
}

object ModifySpec extends BaseSpec("Modify") {
  implicit val arbitraryModifyInt: Arbitrary[Modify[Int]] = Arbitrary {
    for (endo <- arbitrary[Int => Int]) yield Modify[Int](endo)
  }

  implicit val equalModifyInt: Equal[Modify[Int]] =
    Equal.equalBy[Modify[Int], Int](mi => mi.apply(0) + mi.apply(1) + mi.apply(100))

  checkAll(invariantFunctor.laws[Modify])
}
