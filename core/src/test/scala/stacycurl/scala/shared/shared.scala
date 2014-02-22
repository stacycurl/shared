package stacycurl.scala.shared

import org.junit.Test

import org.junit.Assert._


class SharedTests {
  @Test def canGetInitialValue {
    assertEquals(1, Shared(1).get())
  }
}
