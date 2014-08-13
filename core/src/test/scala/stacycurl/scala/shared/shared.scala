package stacycurl.scala.shared

import java.util.concurrent.atomic.AtomicInteger
import org.junit.Test
import scala.collection.immutable.Stack
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scalaz._

import org.junit.Assert._


class SharedTests {
  @Test def canGetInitialValue {
    assertEquals("initial", Shared("initial").get())
  }

  @Test def modifyReturnsChange {
    assertEquals(Change("initial", "initial >> modified"),
      Shared("initial").modify(_ ++ " >> modified"))
  }

  @Test(timeout = 1000) def modifyIsThreadSafe {
    val shared = Shared("initial")

    val modifiers = Random.shuffle(
      threads(10, shared.modify("<" ++ _)) ++ threads(10, shared.modify(_ ++ ">"))
    )

    modifiers.foreach(_.start())
    modifiers.foreach(_.join())

    assertEquals("<<<<<<<<<<initial>>>>>>>>>>", shared.get())
  }

  @Test(timeout = 1000) def zippedLockIsThreadSafe {
    val (left, middle, right) = (Shared(0), Shared(0.0), Shared(""))
    val (lmr, rml) = (left.zip(middle).zip(right), right.zip(middle).zip(left))

    val modifiers = Random.shuffle(
      threads(10, lmr.modify { case ((i, d), s) => ((i + 1,   d), s) }) ++
      threads(10, rml.modify { case ((s, d), i) => ((s + ">", d), i) })
    )

    modifiers.foreach(_.start())
    modifiers.foreach(_.join())

    assertEquals(10, left.get())
    assertEquals(">>>>>>>>>>", right.get())
  }

  @Test def canSet {
    val shared = Shared(1)
    shared.value = 2

    assertEquals(2, shared.get())

    shared.set(3)

    assertEquals(3, shared.get())
  }

  @Test def withValueTemporarilySetsValue {
    val int = Shared(1)

    val result = int.withValue(2) {
      int.get() + 10
    }

    assertEquals(12, result)
    assertEquals(1, int.get())
  }

  @Test def updateIsModifyWithActions {
    val boolean = Shared(new AtomicInteger(1)) // perverse but just want an example

    boolean.update(_.incrementAndGet())

    assertEquals(2, boolean.get().get())
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

  @Test def canFilter {
    val int = Shared(1)
    val bigInt = int.filter(ci => math.abs(ci.delta) > 1)
    val bigIntChanges = bigInt.changes()

    // The filter cannot constrain the original
    int += 1
    assertEquals(2, bigInt.get())
    assertEquals(List(Change(1, 2)), bigIntChanges.get())

    // But it should constrain the 'view'
    bigInt += 1
    assertEquals(2, bigInt.get())
    assertEquals(List(Change(1, 2)), bigIntChanges.get())
    bigInt += 2
    assertEquals(4, bigInt.get())
    assertEquals(List(Change(1, 2), Change(2, 4)), bigIntChanges.get())
  }

  @Test def sharedListBehavesLikeListBuffer {
    val list = Shared(List("initial"))

    list += "new"

    assertEquals(List("initial", "new"), list.result())

    list.clear()

    assertEquals(Nil, list.get())
  }

  @Test def sharedMapBehavesLikeMapBuilder {
    val map: Shared[Map[Int, String]] = Shared(Map(1 -> "one"))

    map += (2 -> "two")

    assertEquals(Map(1 -> "one", 2 -> "two"), map.result())
    assertEquals(Some("one"), map.get(1))
    assertEquals(None, map.get(3))
    map.clear()
    assertEquals(Map.empty[Int, String], map.result())
  }

  @Test def sharedNumeric {
    val si = Shared(1)
    val changes = si.changes()

    si += 1
    si *= 3
    si -= 1

    assertEquals(List(1, 2, 6, 5), changes.values())
  }

  @Test def sharedFractional {
    val si = Shared(6.0)

    si /= 3.0

    assertEquals(2.0, si.get(), 1e-6)
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

    string.value = "bar"; Thread.sleep(500)

    assertEquals(0, doneAwaiting.getCount())
  }

  @Test def awaitCanTimeout {
    val string = Shared("foo")

    assertEquals(None, string.await(_ == "bar", 1))
  }

  @Test def canNotifyOnChange {
    val int = Shared(1)
    val intChanges = int.changes()

    assertEquals(Nil, intChanges.get())

    int.value = 2

    assertEquals(Change.many(1, 2), intChanges.get())
  }

  @Test def canFilterChanges {
    val int = Shared(1)
    val bigChanges = int.changes(ci => math.abs(ci.delta) > 1)

    List(2, 10, 11, 9, 7, 2, 1).foreach(int.value = _)

    assertEquals(Changes.many(1, 10, 7, 2).get(), bigChanges.get())
  }

  @Test def xmappedSharedCanNotifyOnChange {
    val int    = Shared(1)
    val double = int.xmap[Double](_.toDouble, _.toInt)

    val intChanges    = int.changes()
    val doubleChanges = double.changes()

    assertEquals(Nil, doubleChanges.get())

    int.value = 2

    assertEquals(Change.many(1, 2),     intChanges.get())
    assertEquals(Change.many(1.0, 2.0), doubleChanges.get())

    double.value = 3.0

    assertEquals(Change.many(1, 2, 3),       intChanges.get())
    assertEquals(Change.many(1.0, 2.0, 3.0), doubleChanges.get())
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

  @Test def zipSharedCanNotifyOnChange {
    val int    = Shared(1)
    val string = Shared("one")
    val tuple  = int.zip(string)

    val intChanges    = int.changes()
    val stringChanges = string.changes()
    val tupleChanges  = tuple.changes()

    assertEquals(Nil, tupleChanges.get())

    int.value = 2

    assertEquals(Change.many((1, "one"), (2, "one")), tupleChanges.get())

    string.value = "two"

    assertEquals(Change.many((1, "one"), (2, "one"), (2, "two")), tupleChanges.get())

    tuple.value = (3, "three")

    assertEquals(Change.many(1, 2, 3),                                          intChanges.get())
    assertEquals(Change.many("one", "two", "three"),                            stringChanges.get())
    assertEquals(Change.many((1, "one"), (2, "one"), (2, "two"), (3, "three")), tupleChanges.get())
  }

  @Test def canClearChanges {
    val int = Shared(1)
    val changes = int.changes()

    int.value = 2

    assertEquals(Change.many(1, 2), changes.get())

    changes.clear()

    assertEquals(Nil, changes.get())
  }

  private def threads[Discard](count: Int, f: => Discard): List[Thread] =
    List.fill(count)(thread(f))

  private def thread[Discard](f: => Discard): Thread = new Thread {
    override def run() = f
  }

  private val first  = Lens.firstLens[String, Int]
  private val second = Lens.secondLens[String, Int]
}
