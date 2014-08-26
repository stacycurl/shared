package sjc.shared

import org.junit.Test
import org.scalacheck._
import scalaz.Equal


abstract class BaseSpec(name: String) extends Properties(name) {
  protected implicit val int: Arbitrary[Int] = Arbitrary(Gen.choose(1, 100))
  protected implicit val equalInt: Equal[Int] = Equal.equalA[Int]

  protected implicit val intEndo: Arbitrary[Int => Int] = Arbitrary(Gen.oneOf(
    intEndo("times 2", _ * 2), intEndo("plus 2", _ + 2),
    intEndo("minus 2", _ - 2), intEndo("negate", -_)
  ))

  protected def checkAll(props: Properties): Unit =
    for ((name, prop) <- props.properties) yield property(name) = prop

  protected def arbitrary[A: Arbitrary] = implicitly[Arbitrary[A]].arbitrary

  private def intEndo(name: String, f: Int => Int): Int => Int = new NamedFunction[Int, Int](name, f)

  protected class NamedFunction[A, B](name: String, f: A => B) extends (A => B) {
    def apply(a: A): B = f(a)
    override def toString = name
  }
}

