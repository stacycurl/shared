package stacycurl.scala.shared

import org.junit.Test

import org.junit.Assert._


class ChangesTests {
  @Test def many {
    assertEquals(Nil, Changes.many().get())
    assertEquals(Nil, Changes.many(1).get())
    assertEquals(List(Change(1, 2)), Changes.many(1, 2).get())
    assertEquals(List(Change(1, 2), Change(2, 3), Change(3, 4)), Changes.many(1, 2, 3, 4).get())
  }

  @Test def values {
    assertEquals(Nil, Changes.many().values())
    assertEquals(Nil, Changes.many(1).values())
    assertEquals(List(1, 2), Changes.many(1, 2).values())
    assertEquals(List(1, 2, 3), Changes.many(1, 2, 3).values())
  }

  @Test def canMapOver {
    assertEquals(List(Change("1", "2")),
      Changes.many(1, 2).map[String]((i: Int) => i.toString).get())
  }

  @Test def canFlatMap {
    // Todo check functor laws
    assertEquals(List(Change(-2, 2), Change(-3, 3)),
      Changes.many(1, 2, 3).flatMap(i => Changes.many(-i, i)).get())
  }

  @Test def canZip {
    assertEquals(List(Change((1, "one"), (2, "two"))),
      Changes.many(1, 2).zip(Changes.many("one", "two")).get())
  }

  @Test def canFilter {
    assertEquals(Nil, Changes.many().get())
    assertEquals(Nil, Changes.many(1, 2).filter(_ => false).get())
    assertEquals(Nil, Changes.many(1, 2, 3).filter(_ => false).get())
    assertEquals(List(Change(1, 2)), Changes.many(1, 2).filter(_ => true).get())

    assertEquals(Changes.many(1, 3, 5).get(),
      Changes.many(1, 2, 3, 4, 5).filter(ci => (ci.after - ci.before) > 1).get())

    assertEquals(Changes.many(1, 10, 7, 2).get(),
      Changes.many(1, 2, 10, 11, 9, 7, 2, 1).filter(ci => math.abs(ci.after - ci.before) > 1).get())
  }
}
