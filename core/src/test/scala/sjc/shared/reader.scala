package sjc.shared

import org.junit.Test
import org.scalacheck._
import scala.collection.immutable.Stack
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scalaz._

import org.junit.Assert._


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

object ReaderSpec extends BaseSpec("Reader") {
  import scalaz.scalacheck.ScalazProperties._

  implicit def arbitraryReadep[A](implicit arbA: Arbitrary[A]): Arbitrary[Reader[A]] = Arbitrary {
    for (a <- arbitrary[A]) yield FunctionReader[A](() => a)
  }

  checkAll(comonad.laws[Reader])
  checkAll(monad.laws[Reader])
  checkAll(traverse.laws[Reader])
  checkAll(zip.laws[Reader])
}
