package stacycurl.scala.shared

import org.junit.Test
import scala.collection.immutable.Stack
import scala.util.Random

import org.junit.Assert._
import scalaz._


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

  @Test def modifyAndCalcPerformsCalculateOnOldAndModifiedValue {
    val shared = Shared("initial")

    assertEquals(List("initial", "initial >> modified"),
      shared.modifyAndCalc(_ + " >> modified") {
        case (initial, modified) => List(initial, modified)
      })

    assertEquals("initial >> modified", shared.get())
  }

  @Test def reifiedModifyBehavesTheSameAsNormal {
    val modify = Modify[String](_ ++ " >> modified")
    val shared = Shared("initial")

    assertEquals("initial", shared.modify(modify))
    assertEquals("initial >> modified", shared.get())
  }

  @Test def reifiedModifyAndGetBehavesTheSameAsNormal {
    val modifyAndGet = ModifyAndGet[String](_ ++ " >> modified")

    assertEquals("initial >> modified", Shared("initial").modify(modifyAndGet))
  }

  @Test def reifiedModifyAndCalcBehavesTheSameAsNormal {
    val shared = Shared("initial")

    val modifyAndCalc = ModifyAndCalc[String, List[String]](_ ++ " >> modified", {
      case (initial, modified) => List(initial, modified)
    })

    assertEquals(List("initial", "initial >> modified"), shared.modify(modifyAndCalc))
    assertEquals("initial >> modified", shared.get())
  }

  @Test def canXmap {
    val string = Shared("initial")
    val reversed = string.xmap[String](_.reverse, _.reverse)

    assertEquals(string.get().reverse, reversed.get())

    reversed.modify("deifidom >> " + _)
    assertEquals("initial >> modified", string.get())
  }

  @Test def canCreateLens {
    val tuple  = Shared(("one", 1))
    val string = tuple.lens(Lens.firstLens[String, Int])
    val int    = tuple.lens(Lens.secondLens[String, Int])

    assertEquals("one", string.get())
    assertEquals(1,     int.get())

    assertEquals("one", string.modify(_ ++ " >> two"))
    assertEquals(1,     int.modify(_ + 1))

    assertEquals("one >> two", string.get())
    assertEquals(2,            int.get())

    assertEquals(("one >> two", 2), tuple.get())
  }

  @Test def sharedListBehavesLikeListBuffer {
    val list = Shared(List("initial"))

    list += "new"

    assertEquals(List("initial", "new"), list.get())

    list.clear()

    assertEquals(Nil, list.get())
  }

  @Test def canGetSortedViewOfAnySeq {
    val list  = Shared(List(1, 3, 2))
    val stack = Shared(Stack(1, 3, 2))

    assertEquals(list.get().sorted,  list.sorted.get())
    assertEquals(stack.get().sorted, stack.sorted.get())
  }

  @Test def canGetSortByViewOfAnySeq {
    val list  = Shared(List("aa", "bbb", "c"))
    val stack = Shared(Stack("aa", "bbb", "c"))

    assertEquals(list.get().sortBy(_.length),  list.sortBy(_.length).get())
    assertEquals(stack.get().sortBy(_.length), stack.sortBy(_.length).get())
  }

  @Test def canGetSortWithViewOfAnySeq {
    val list  = Shared(List(1, 3, 2))
    val stack = Shared(Stack(1, 3, 2))

    assertEquals(list.get().sortWith(_ > _),  list.sortWith(_ > _).get())
    assertEquals(stack.get().sortWith(_ > _), stack.sortWith(_ > _).get())
  }

  @Test def canGetTransformedViewOfAnySeq {
    val list  = Shared(List(1, 3, 2))
    val stack = Shared(Stack(1, 3, 2))

    assertEquals(list.get().padTo(5, 0),  list.transform(_.padTo(5, 0)).get())
    assertEquals(stack.get().padTo(5, 0), stack.transform(_.padTo(5, 0)).get())
  }

  @Test def canGetReadOnlyView {
    assertEquals("value", Shared("value").reader.get())
  }

  @Test def canMapOverReader {
    assertEquals("321", Shared("123").reader.map(_.reverse).get())
  }

  @Test def canMapOverUpdate {
    val modifyAndGet = ModifyAndGet[Int](_ + 1)

    assertEquals(Shared(1).modify(modifyAndGet) * 10, Shared(1).modify(modifyAndGet.map(_ * 10)))
  }

  private def threads[Discard](count: Int, f: => Discard): List[Thread] =
    List.fill(count)(thread(f))

  private def thread[Discard](f: => Discard): Thread = new Thread {
    override def run() = f
  }
}
