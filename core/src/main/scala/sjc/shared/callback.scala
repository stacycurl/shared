package sjc.shared


object Callback {
  def apply[A](callback: Change[A] => Unit): Callback[A] = SingleCallback[A](callback)
}

trait Callback[A] extends (Change[A] => Change[A]) {
  def contramap[B](bToA: B => A): Callback[B] =
    Callback[B]((changeB: Change[B]) => apply(changeB.map(bToA)))

  def guard(condition: () => Boolean): Callback[A] =
    filter(_ => condition())

  def filter(p: Change[A] => Boolean): Callback[A] =
    Callback[A]((changeA: Change[A]) => if (p(changeA)) apply(changeA))
}

case class SingleCallback[A](value: Change[A] => Unit) extends Callback[A] {
  def apply(change: Change[A]): Change[A] = change.perform(value)
}

class Callbacks[A] extends Callback[A] {
  def apply(change: Change[A]): Change[A] = {
    callbacks.foreach(callback => callback.apply(change))
    change
  }

  def +=(callback: Callback[A]) = callbacks += callback

  private lazy val callbacks: Shared[List[Callback[A]]] = Shared(Nil)
}
