package stacycurl.scala.shared

import org.junit.Test
import scala.collection.immutable.Stack
import scala.collection.mutable.ListBuffer
import scala.util.Random

import org.junit.Assert._
import scalaz._


class SharedTests {
  @Test def canGetInitialValue {
    assertEquals("initial", Shared("initial").get())
  }

  @Test def modifyReturnsChange {
    assertEquals(Change("initial", "initial >> modified"),
      Shared("initial").modify(_ ++ " >> modified"))
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

  @Test def canXmap {
    val string   = Shared("initial")
    val reversed = string.xmap[String](_.reverse, _.reverse)

    assertEquals(string.get().reverse, reversed.get())

    assertEquals(Change("laitini", "deifidom >> laitini"), reversed.modify("deifidom >> " + _))
    assertEquals("initial >> modified", string.get())
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
  }

  @Test def canZip {
    val string = Shared("one")
    val int    = Shared(1)
    val tuple  = string.zip(int)

    assertEquals(("one", 1), tuple.get())

    assertEquals(Change(("one", 1), ("two", 2)), tuple.modify(_ => ("two", 2)))

    assertEquals("two", string.get())
    assertEquals(2,     int.get())
  }

  @Test def sharedListBehavesLikeListBuffer {
    val list = Shared(List("initial"))

    list += "new"

    assertEquals(List("initial", "new"), list.get())

    list.clear()

    assertEquals(Nil, list.get())
  }

  @Test def sharedMapBehavesListMapBuilder {
    val map: Shared[Map[Int, String]] = Shared(Map(1 -> "one"))

    map += (2 -> "two")

    assertEquals(Map(1 -> "one", 2 -> "two"), map.get())
    assertEquals(Some("one"), map.get(1))
    assertEquals(None, map.get(3))
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

  @Test def canAwaitForPredicate {
    val string = Shared("foo")
    assertEquals(Some("foo"), string.await(_ == "foo"))

    val doneAwaiting = new java.util.concurrent.CountDownLatch(1)
    val started = new java.util.concurrent.CountDownLatch(1)

    thread {
      started.countDown()

      string.await(_ == "bar")

      doneAwaiting.countDown()
    }.start()

    started.await(); Thread.sleep(500)

    assertEquals(1, doneAwaiting.getCount())

    string.modify(_ => "bar"); Thread.sleep(500)

    assertEquals(0, doneAwaiting.getCount())
  }

  @Test def awaitCanTimeout {
    val string = Shared("foo")

    assertEquals(None, string.await(_ == "bar", 1))
  }

  @Test def canNotifyOnChange {
    val int = Shared(1)
    val intChanges = int.changes

    assertEquals(Nil, intChanges.get())

    int.modify(_ => 2)

    assertEquals(Change.many(1, 2), intChanges.get())
  }

  @Test def xmappedSharedCanNotifyOnChange {
    val int    = Shared(1)
    val double = int.xmap[Double](_.toDouble, _.toInt)

    val intChanges    = int.changes
    val doubleChanges = double.changes

    assertEquals(Nil, doubleChanges.get())

    int.modify(_ => 2)

    assertEquals(Change.many(1, 2),     intChanges.get())
    assertEquals(Change.many(1.0, 2.0), doubleChanges.get())

    double.modify(_ => 3.0)

    assertEquals(Change.many(1, 2, 3),       intChanges.get())
    assertEquals(Change.many(1.0, 2.0, 3.0), doubleChanges.get())
  }

  @Test def lensSharedCanNotifyOnChange {
    val tuple  = Shared(("one", 1))
    val string = tuple.lens(first)

    val tupleChanges  = tuple.changes
    val stringChanges = string.changes

    assertEquals(Nil, stringChanges.get())

    tuple.modify(_ => ("two", 2))

    assertEquals(Change.many(("one", 1), ("two", 2)), tupleChanges.get())
    assertEquals(Change.many("one", "two"),           stringChanges.get())

    string.modify(_ => "three")

    assertEquals(Change.many(("one", 1), ("two", 2), ("three", 2)), tupleChanges.get())
    assertEquals(Change.many("one", "two", "three"),                stringChanges.get())
  }

  @Test def zipSharedCanNotifyOnChange {
    val int    = Shared(1)
    val string = Shared("one")
    val tuple  = int.zip(string)

    val intChanges    = int.changes
    val stringChanges = string.changes
    val tupleChanges  = tuple.changes

    assertEquals(Nil, tupleChanges.get())

    int.modify(_ => 2)

    assertEquals(Change.many((1, "one"), (2, "one")), tupleChanges.get())

    string.modify(_ => "two")

    assertEquals(Change.many((1, "one"), (2, "one"), (2, "two")), tupleChanges.get())

    tuple.modify(_ => (3, "three"))

    assertEquals(Change.many(1, 2, 3),                                          intChanges.get())
    assertEquals(Change.many("one", "two", "three"),                            stringChanges.get())
    assertEquals(Change.many((1, "one"), (2, "one"), (2, "two"), (3, "three")), tupleChanges.get())
  }

  private def threads[Discard](count: Int, f: => Discard): List[Thread] =
    List.fill(count)(thread(f))

  private def thread[Discard](f: => Discard): Thread = new Thread {
    override def run() = f
  }

  private val first  = Lens.firstLens[String, Int]
  private val second = Lens.secondLens[String, Int]
}

class ChangeTests {
  @Test def calcPerformsCalculateOnOldAndModifiedValue {
    assertEquals(List("initial", "initial >> modified"),
      Change("initial", "initial >> modified").calc {
        case (initial, modified) => List(initial, modified)
      }
    )
  }

  @Test def canMapOver {
    assertEquals(Change("1", "2"), Change(1, 2).map(_.toString))
  }

  @Test def canZip {
    assertEquals(Change(("one", 1), ("two", 2)), Change("one", "two").zip(Change(1, 2)))
  }
}

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

class ReaderTests {
  @Test def canCastReaderToValue {
    assertEquals(3, (reader(3): Int))
  }

  @Test def canMapOverReader {
    assertEquals("321", reader("123").map(_.reverse).get())
  }

  @Test def canZip {
    assertEquals(("one", 1), reader("one").zip(reader(1)).get())
  }

  private def reader[A](a: A) = new Reader[A] { def get() = a }
}
