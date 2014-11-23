package sjc.shared

import java.util.concurrent.atomic.AtomicInteger
import org.junit.Test
import scala.collection.immutable.Stack
import scala.util.Random

import org.junit.Assert._


class SharedTests {
  @Test def canGetInitialValue(): Unit = {
    assertEquals("initial", Shared("initial").get())
  }

  @Test def modifyReturnsChange(): Unit = {
    assertEquals(Change("initial", "initial >> modified"),
      Shared("initial").modify(_ ++ " >> modified"))
  }

  @Test(timeout = 1000) def modifyIsThreadSafe(): Unit = {
    val shared = Shared("initial")

    val modifiers = Random.shuffle(
      threads(10, shared.modify("<" ++ _)) ++ threads(10, shared.modify(_ ++ ">"))
    )

    modifiers.foreach(_.start())
    modifiers.foreach(_.join())

    assertEquals("<<<<<<<<<<initial>>>>>>>>>>", shared.get())
  }

  @Test(timeout = 1000) def zippedLockIsThreadSafe(): Unit = {
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

  @Test def canSet(): Unit = {
    val shared = Shared(1)
    shared.value = 2

    assertEquals(2, shared.get())

    shared.set(3)

    assertEquals(3, shared.get())
  }

  @Test def withValueTemporarilySetsValue(): Unit = {
    val int = Shared(1)

    val result = int.withValue(2) {
      int.get() + 10
    }

    assertEquals(12, result)
    assertEquals(1, int.get())
  }

  @Test def updateIsModifyWithActions(): Unit = {
    val boolean = Shared(new AtomicInteger(1)) // perverse but just want an example

    boolean.update(_.incrementAndGet())

    assertEquals(2, boolean.get().get())
  }

  @Test def canXmap(): Unit = {
    val string   = Shared("initial")
    val reversed = string.xmap[String](_.reverse, _.reverse)

    assertEquals(string.get().reverse, reversed.get())

    assertEquals(Change("laitini", "deifidom >> laitini"), reversed.modify("deifidom >> " + _))
    assertEquals("initial >> modified", string.get())

    assertSame(string.lock, reversed.lock)
  }

  @Test def canZip(): Unit = {
    val string = Shared("one")
    val int    = Shared(1)
    val tuple  = string.zip(int)

    assertEquals(("one", 1), tuple.get())

    assertEquals(Change(("one", 1), ("two", 2)), tuple.modify(_ => ("two", 2)))

    assertEquals("two", string.get())
    assertEquals(2,     int.get())
  }

  @Test def canFilter(): Unit = {
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

    assertSame(int.lock, bigInt.lock)
  }

  @Test def sharedListBehavesLikeListBuffer(): Unit = {
    val list = Shared(List("initial"))

    list += "new"

    assertEquals(List("initial", "new"), list.result())

    list.clear()

    assertEquals(Nil, list.get())

    list += "new"
    list += "newer"

    assertEquals(List("new", "newer"), list.drain())
    assertEquals(Nil, list.drain())
  }

  @Test def sharedMapBehavesLikeMapBuilder(): Unit = {
    val map: Shared[Map[Int, String]] = Shared(Map(1 -> "one"))

    map += (2 -> "two")

    assertEquals(Map(1 -> "one", 2 -> "two"), map.result())
    assertEquals(Some("one"), map.get(1))
    assertEquals(None, map.get(3))
    map.clear()
    assertEquals(Map.empty[Int, String], map.result())
  }

  @Test def sharedNumeric(): Unit = {
    val si = Shared(1)
    val changes = si.changes()

    si += 1
    si *= 3
    si -= 1

    assertEquals(List(1, 2, 6, 5), changes.values())
  }

  @Test def sharedFractional(): Unit = {
    val si = Shared(6.0)

    si /= 3.0

    assertEquals(2.0, si.get(), 1e-6)
  }

  @Test def canGetSortedViewOfAnySeq(): Unit = {
    val list  = Shared(List(1, 3, 2))
    val stack = Shared(Stack(1, 3, 2))

    assertEquals(list.get().sorted,  list.sorted.get())
    assertEquals(stack.get().sorted, stack.sorted.get())
  }

  @Test def canGetSortByViewOfAnySeq(): Unit = {
    val list  = Shared(List("aa", "bbb", "c"))
    val stack = Shared(Stack("aa", "bbb", "c"))

    assertEquals(list.get().sortBy(_.length),  list.sortBy(_.length).get())
    assertEquals(stack.get().sortBy(_.length), stack.sortBy(_.length).get())
  }

  @Test def canGetSortWithViewOfAnySeq(): Unit = {
    val list  = Shared(List(1, 3, 2))
    val stack = Shared(Stack(1, 3, 2))

    assertEquals(list.get().sortWith(_ > _),  list.sortWith(_ > _).get())
    assertEquals(stack.get().sortWith(_ > _), stack.sortWith(_ > _).get())
  }

  @Test def canGetTransformedViewOfAnySeq(): Unit = {
    val list  = Shared(List(1, 3, 2))
    val stack = Shared(Stack(1, 3, 2))
    val paddedList  = list.transform(_.padTo(5, 0))
    val paddedStack = stack.transform(_.padTo(5, 0))

    assertEquals(list.get().padTo(5, 0),  paddedList.get())
    assertEquals(stack.get().padTo(5, 0), paddedStack.get())

    paddedList.value  = List(4)
    paddedStack.value = Stack(4)

    assertEquals(List(4), list.get())
    assertEquals(Stack(4), stack.get())
  }

  @Test def canAwaitForPredicate(): Unit = {
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

  @Test def awaitCanTimeout(): Unit = {
    val string = Shared("foo")

    assertEquals(None, string.await(_ == "bar", 1))
  }

  @Test def canNotifyOnChange(): Unit = {
    val int = Shared(1)
    val intChanges = int.changes()

    assertEquals(Nil, intChanges.get())

    int.value = 2

    assertEquals(Change.many(1, 2), intChanges.get())
  }

  @Test def canFilterChanges(): Unit = {
    val int = Shared(1)
    val bigChanges = int.changes(ci => math.abs(ci.delta) > 1)

    List(2, 10, 11, 9, 7, 2, 1).foreach(int.value = _)

    assertEquals(Changes.many(1, 10, 7, 2).get(), bigChanges.get())
  }

  @Test def xmappedSharedCanNotifyOnChange(): Unit = {
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

  @Test def zipSharedCanNotifyOnChange(): Unit = {
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

  @Test def canClearChanges(): Unit = {
    val int = Shared(1)
    val changes = int.changes()

    int.value = 2

    assertEquals(Change.many(1, 2), changes.get())

    changes.clear()

    assertEquals(Nil, changes.get())
  }

  @Test def threadLocalShared(): Unit = {
    val shared = Shared.threadLocal("initial", new Synchronized)
    val changes = shared.changes()
    val results = Shared[Map[String, String]](Map.empty)

    def appender(append: String) = thread {
      for { _ <- Range(0, 10) } shared.modify(_ ++ append).after

      results += ((append, shared.get()))
    }

    val modifiers = List(appender("1"), appender("2")) ++ threads(10, shared.modify("<" ++ _))

    modifiers.foreach(_.start())
    modifiers.foreach(_.join())

    assertEquals(Map("1" -> "initial1111111111", "2" -> "initial2222222222"), results.get())
    assertEquals(30, changes.get().size)
  }

  private def threads[Discard](count: Int, f: => Discard): List[Thread] =
    List.fill(count)(thread(f))

  private def thread[Discard](f: => Discard): Thread = new Thread {
    override def run() = f
  }
}
