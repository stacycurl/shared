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

  @Test def modifyReturnsOldValue {
    val shared = Shared("initial")

    assertEquals("initial", shared.modify(_ ++ " >> modified"))
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

  @Test def canModifyAndGet {
    val shared = Shared("initial")

    assertEquals("initial >> modified", shared.modifyAndGet(_ ++ " >> modified"))
    assertEquals("initial >> modified", shared.get())
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
    val shared = Shared("initial")
    val modify = Modify[String](_ ++ " >> modified")

    assertEquals("initial", shared.modify(modify))
    assertEquals("initial >> modified", shared.get())
  }

  @Test def reifiedModifyAndGetBehavesTheSameAsNormal {
    val shared = Shared("initial")
    val modifyAndGet = ModifyAndGet[String](_ ++ " >> modified")

    assertEquals("initial >> modified", shared.modify(modifyAndGet))
    assertEquals("initial >> modified", shared.get())
  }

  @Test def reifiedModifyAndCalcBehavesTheSameAsNormal {
    val shared = Shared("initial")

    val modifyAndCalc = ModifyAndCalc[String, List[String]](_ ++ " >> modified", {
      case (initial, modified) => List(initial, modified)
    })

    assertEquals(List("initial", "initial >> modified"), shared.modify(modifyAndCalc))
    assertEquals("initial >> modified", shared.get())
  }

  @Test def canConvertModifyToModifyAndGet {
    val shared = Shared("initial")
    val modify = Modify[String](_ ++ " >> modified")

    assertEquals("initial >> modified", shared.modify(modify.andGet))
    assertEquals("initial >> modified", shared.get())
  }

  @Test def canConvertModifyToModifyAndCalc {
    val shared = Shared("initial")
    val modify = Modify[String](_ ++ " >> modified")

    assertEquals(List("initial", "initial >> modified"), shared.modify(modify.andCalc {
      case (initial, modified) => List(initial, modified)
    }))

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
    val string = tuple.lens(first)
    val int    = tuple.lens(second)

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

  @Test def canXMapOverUpdate {
    val appendTwo = Modify[List[Int]](_ ++ List(2))

    assertEquals(Shared(List(0, 1)).xmap[List[Int]](_.reverse, _.reverse).modify(appendTwo),
      Shared(List(0, 1)).modify(appendTwo.xmap[List[Int]](_.reverse, _.reverse)))
  }

  @Test def anUpdateCanBeAppliedToAPartOfAnotherShared {
    val addOne = ModifyAndGet[Int](_ + 1)

    assertEquals(Shared("one", 1).lens(second).modify(addOne),
      Shared("one", 1).modify(addOne.lens(second)))
  }

  private def threads[Discard](count: Int, f: => Discard): List[Thread] =
    List.fill(count)(thread(f))

  private def thread[Discard](f: => Discard): Thread = new Thread {
    override def run() = f
  }

  private val first  = Lens.firstLens[String, Int]
  private val second = Lens.secondLens[String, Int]
}
