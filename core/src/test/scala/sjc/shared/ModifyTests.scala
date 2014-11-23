package sjc.shared

import java.util.concurrent.atomic.AtomicInteger
import org.junit.Test

import org.junit.Assert._


class ModifyTests {
  @Test def behavesTheSameAsUnreifiedModify(): Unit = {
    assertEquals(Change("initial", "initial >> modified"),
      Shared("initial").modify(Modify[String](_ ++ " >> modified")))
  }

  @Test def canXMapOverModify(): Unit = {
    val addOne: Modify[Int]     = Modify[Int](_ + 1)
    val addOneS: Modify[String] = addOne.xmap[String](_.toString, _.toInt)

    assertEquals(Change("1", "2"), Shared("1").modify(addOneS))
  }

  @Test def canZip(): Unit = {
    val append = Modify[String](_ => "two")
    val add    = Modify[Int](_ => 2)
    val zipped = append.zip(add)

    assertEquals(Change(("one", 1), ("two", 2)), Shared(("one", 1)).modify(zipped))
  }

  @Test def updateIsLikeModifyButForActions(): Unit = {
    val boolean = Shared(new AtomicInteger(1)) // perverse but I just want an example

    val addOne = Update[AtomicInteger](_.incrementAndGet())

    boolean.modify(addOne)

    assertEquals(2, boolean.get().get())
  }
}
