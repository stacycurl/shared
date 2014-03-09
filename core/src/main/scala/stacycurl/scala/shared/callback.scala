package stacycurl.scala.shared

import scalaz._


object Callback {
  def apply[A](callback: Change[A] => Unit): Callback[A] = SingleCallback[A](callback)

  implicit object CallbackContravariant extends Contravariant[Callback] {
    def contramap[A, B](ca: Callback[A])(f: B => A): Callback[B] = ca.contramap(f)
  }
}

trait Callback[A] extends (Change[A] => Change[A]) {
  def contramap[B](bToA: B => A): Callback[B] =
    Callback[B]((changeB: Change[B]) => apply(changeB.map(bToA)))

  def guard(condition: Shared[Boolean]): Callback[A] =
    Callback[A]((changeA: Change[A]) => if (condition.get()) apply(changeA))
}

case class SingleCallback[A](value: Change[A] => Unit) extends Callback[A] {
  def apply(change: Change[A]): Change[A] = { value(change); change }
}

class Callbacks[A] extends Callback[A] {
  def apply(change: Change[A]): Change[A] = {
    callbacks.foreach(callback => callback.apply(change))
    change
  }

  def +=(callback: Callback[A]) = callbacks += callback

  private lazy val callbacks: Shared[List[Callback[A]]] = Shared(Nil)
}
