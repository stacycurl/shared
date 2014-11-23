package sjc.shared

import org.scalacheck._
import scalaz._

import scalaz.scalacheck.ScalazProperties._
import sjc.shared.instances.callback._
import sjc.shared.instances.change._


object CallbackSpec extends BaseSpec("Callback") {
  implicit val intCallback: Arbitrary[Callback[Int]] = Arbitrary {
    Gen.const(Callback[Int](_ => ()))
  }

  implicit def equalIntCallback: Equal[Callback[Int]] =
    Equal.equalBy[Callback[Int], Change[Int]](_.apply(Change(123, 456)))

  checkAll(contravariant.laws[Callback])
}
