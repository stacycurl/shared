package stacycurl.scala.shared

import org.junit.Test

import org.junit.Assert._
import scalaz._


class ModifyTests {
  @Test def behavesTheSameAsUnreifiedModify {
    assertEquals(Change("initial", "initial >> modified"),
      Shared("initial").modify(Modify[String](_ ++ " >> modified")))
  }

  @Test def canXMapOverModify {
    val addOne: Modify[Int]     = Modify[Int](_ + 1)
    val addOneS: Modify[String] = addOne.xmap[String](_.toString, _.toInt)

    assertEquals(Change("1", "2"), Shared("1").modify(addOneS))
  }

  @Test def modifyCanBeAppliedToAPartOfAnotherShared {
    assertEquals(Change(("one", 1), ("one", 2)),
      Shared(("one", 1)).modify(Modify[Int](_ + 1).lens(second)))
  }

  @Test def canZip {
    val append = Modify[String](_ => "two")
    val add    = Modify[Int](_ => 2)
    val zipped = append.zip(add)

    assertEquals(Change(("one", 1), ("two", 2)), Shared(("one", 1)).modify(zipped))
  }

  private val second = Lens.secondLens[String, Int]
}
