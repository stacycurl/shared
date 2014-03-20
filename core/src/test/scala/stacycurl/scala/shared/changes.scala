package stacycurl.scala.shared

import org.junit.Test

import org.junit.Assert._


class ChangesTests {
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
}
