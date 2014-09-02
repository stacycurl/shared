package sjc.shared

import org.junit.Test
import org.scalacheck._
import scalaz._

import org.junit.Assert._
import scalaz.scalacheck.ScalazProperties._
import sjc.shared.instances.reader._


class ReaderTests {
  @Test def canShow {
    implicit val showInt: Show[Int] = Show.shows[Int](i => s"Int: $i")

    assertEquals("Int: 123", readerShow[Int].shows(reader(123)))
  }

  @Test def canUnzip {
    val (string, int) = ReaderInstance.unzip(reader(("one", 1)))

    assertEquals("one", string.get())
    assertEquals(1, int.get())
  }

  @Test def canCoZip {
    def assertCanCozip(input: String \/ Int) {
      val result: Reader[String] \/ Reader[Int] = ReaderInstance.cozip(reader(input))

      assertEquals(input, result.bimap(_.get(), _.get()))
    }

    assertCanCozip(-\/("one"))
    assertCanCozip(\/-(1))
  }

  @Test def representable {
    assertEquals("one", ReaderRepresentable.rep(_ => "one").get())
    assertEquals("one", ReaderRepresentable.unrep(reader("one")).apply(()))
  }

  private def reader[A](a: A) = new Reader[A] { def get() = a }
}

object ReaderSpec extends BaseSpec("Reader") {

  implicit def arbitraryReadep[A](implicit arbA: Arbitrary[A]): Arbitrary[Reader[A]] = Arbitrary {
    for (a <- arbitrary[A]) yield FunctionReader[A](() => a)
  }

  checkAll(comonad.laws[Reader])
  checkAll(monad.laws[Reader])
  checkAll(traverse.laws[Reader])
  checkAll(zip.laws[Reader])
}
