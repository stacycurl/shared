package sjc.shared

import org.junit.Test

import org.junit.Assert._


class ChangesTests {
  @Test def many(): Unit = {
    assertEquals(Nil, Changes.many().get())
    assertEquals(Nil, Changes.many(1).get())
    assertEquals(List(Change(1, 2)), Changes.many(1, 2).get())
    assertEquals(List(Change(1, 2), Change(2, 3), Change(3, 4)), Changes.many(1, 2, 3, 4).get())
  }

  @Test def values(): Unit = {
    assertEquals(Nil, Changes.many().values())
    assertEquals(Nil, Changes.many(1).values())
    assertEquals(List(1, 2), Changes.many(1, 2).values())
    assertEquals(List(1, 2, 3), Changes.many(1, 2, 3).values())
  }

  @Test def canMapOver(): Unit = {
    val original = Changes.many(1, 2)

    assertEquals(List(Change("1", "2")),
      original.map[String]((i: Int) => i.toString).get())

    original.map((i: Int) => i.toString).clear()
    assertEquals(Nil, original.get())
  }

  @Test def canFlatMap(): Unit = {
    // Todo check functor laws
    val original = Changes.many(1, 2, 3)

    assertEquals(List(Change(-2, 2), Change(-3, 3)),
      original.flatMap(i => Changes.many(-i, i)).get())

    original.flatMap(i => Changes.many(-i, i)).clear()
    assertEquals(Nil, original.get())
  }

  @Test def canZip(): Unit = {
    val (left, right) = (Changes.many(1, 2), Changes.many("one", "two"))

    assertEquals(List(Change((1, "one"), (2, "two"))), left.zip(right).get())

    left.zip(right).clear()
    assertEquals(Nil, left.get())
    assertEquals(Nil, right.get())
  }

  @Test def canFilter(): Unit = {
    assertEquals(Nil, Changes.many().get())
    assertEquals(Nil, Changes.many(1, 2).filter(_ => false).get())
    assertEquals(Nil, Changes.many(1, 2, 3).filter(_ => false).get())
    assertEquals(List(Change(1, 2)), Changes.many(1, 2).filter(_ => true).get())

    assertEquals(Changes.many(1, 3, 5).get(),
      Changes.many(1, 2, 3, 4, 5).filter(ci => (ci.after - ci.before) > 1).get())

    assertEquals(Changes.many(1, 10, 7, 2).get(),
      Changes.many(1, 2, 10, 11, 9, 7, 2, 1).filter(ci => math.abs(ci.after - ci.before) > 1).get())

    val unfiltered = Changes.many(1, 2, 3)
    unfiltered.filter(_ => true).clear()

    assertEquals(Nil, unfiltered.get())
  }
}
