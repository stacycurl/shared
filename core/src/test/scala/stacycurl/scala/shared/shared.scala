package stacycurl.scala.shared

import org.junit.Test
import scala.util.Random

import org.junit.Assert._


class SharedTests {
  @Test def canGetInitialValue {
    assertEquals("initial", Shared("initial").get())
  }

  @Test def canModify {
    val shared = Shared("initial")
    shared.modify(_ ++ " >> modified")

    assertEquals("initial >> modified", shared.get())
  }

  @Test def modifyIsThreadSafe {
    val shared = Shared("initial")

    val modifiers = {
      val prefixers = threads(10, shared.modify("<" ++ _))
      val suffixers = threads(10, shared.modify(_ ++ ">"))

      Random.shuffle(prefixers ++ suffixers)
    }

    modifiers.foreach(_.start())
    modifiers.foreach(_.join())

    assertEquals("<<<<<<<<<<initial>>>>>>>>>>", shared.get())
  }

  @Test def modifyReturnsOldValue {
    assertEquals("old", Shared("old").modify(_ => "new"))
  }

  @Test def modifyAndGetReturnsNewValue {
    assertEquals("initial >> modified", Shared("initial").modifyAndGet(_ + " >> modified"))
  }

  @Test def modifyAndCalcModifies {
    val shared = Shared("initial")
    shared.modifyAndCalc(_ ++ " >> modified") { case _ => "ignored" }

    assertEquals("initial >> modified", shared.get())
  }

  @Test def modifyAndCalcPerformsCalculateOnOldAndModifiedValue {
    assertEquals(List("initial", "initial >> modified"),
      Shared("initial").modifyAndCalc(_ + " >> modified") {
        case (initial, modified) => List(initial, modified)
      })
  }

  @Test def reifiedModifyBehavesTheSameAsUnreifiedModify {
    val modify = Modify[String](_ ++ " >> modified")

    val shared = Shared("initial")
    shared.modify(modify)

    assertEquals("initial >> modified", shared.get())
  }

  private def threads[Discard](count: Int, f: => Discard): List[Thread] =
    List.fill(count)(thread(f))

  private def thread[Discard](f: => Discard): Thread = new Thread {
    override def run() = f
  }
}
