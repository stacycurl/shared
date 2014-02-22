package stacycurl.scala.shared

import org.junit.Test

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
}
