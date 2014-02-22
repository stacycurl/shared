package stacycurl.scala.shared

import scalaz._


object Shared {
  implicit object SharedInvariantFunctor extends InvariantFunctor[Shared] {
    def xmap[A, B](sa: Shared[A], aToB: A => B, bToA: B => A): Shared[B] =
      sa.xmap(aToB, bToA)
  }

  def apply[A](initial: A): Shared[A] = SyncShared[A](initial)
}

trait Shared[A] {
  def get(): A
  def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B

  def modify(f: A => A): A       = modifyAndCalc(f) { case (old, _) => old }
  def modifyAndGet(f: A => A): A = modifyAndCalc(f) { case (_, modified) => modified }

  def modify[B](update: Update[A, B]): B = update(this)

  def xmap[B](aToB: A => B, bToA: B => A): Shared[B] =
    XMapShared[A, B](this, aToB, bToA)
}

case class SyncShared[A](initial: A) extends Shared[A] {
  private var value = initial

  // Strange I haven't needed to add synchronized here
  def get(): A = value

  def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B = synchronized {
    val current = value
    value = f(value)

    g(current, value)
  }

}

case class XMapShared[A, B](sa: Shared[A], aToB: A => B, bToA: B => A)
  extends Shared[B] {

  def get(): B = aToB(sa.get())

  def modifyAndCalc[C](f: B => B)(g: (B, B) => C): C = {
    sa.modifyAndCalc(aToB andThen f andThen bToA) {
      case (oldA, newA) => g(aToB(oldA), aToB(newA))
    }
  }
}

trait Update[A, B] {
  def apply(shared: Shared[A]): B
}

case class Modify[A](f: A => A) extends Update[A, A] {
  def apply(shared: Shared[A]): A = shared.modify(f)
}

case class ModifyAndGet[A](f: A => A) extends Update[A, A] {
  def apply(shared: Shared[A]): A = shared.modifyAndGet(f)
}

case class ModifyAndCalc[A, B](f: A => A, g: (A, A) => B) extends Update[A, B] {
  def apply(shared: Shared[A]): B = shared.modifyAndCalc(f)(g)
}
