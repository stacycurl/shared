package sjc.shared

import org.junit.Test
import scala.collection.immutable.Stack
import scala.collection.mutable.ListBuffer
import scala.util.Random

import org.junit.Assert._
import scalaz._


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
