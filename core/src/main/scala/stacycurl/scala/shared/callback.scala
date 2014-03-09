package stacycurl.scala.shared

import scalaz._


object Callback {
  implicit object CallbackContravariant extends Contravariant[Callback] {
    def contramap[A, B](ca: Callback[A])(f: B => A): Callback[B] = ca.contramap(f)
  }
}

case class Callback[A](value: Change[A] => Unit) extends (Change[A] => Unit) {
  def apply(change: Change[A]): Unit = value(change)

  def contramap[B](bToA: B => A): Callback[B] =
    Callback[B]((changeB: Change[B]) => apply(changeB.map(bToA)))

  def guard(condition: Shared[Boolean]): Callback[A] =
    Callback[A]((changeA: Change[A]) => if (condition.get()) apply(changeA))
}

class Callbacks[A] {
  def apply(change: Change[A]): Change[A] = {
    callbacks.foreach(callback => callback.apply(change))
    change
  }

  def +=(callback: Callback[A]) = callbacks += callback

  private lazy val callbacks: Shared[List[Callback[A]]] = Shared(Nil)
}
