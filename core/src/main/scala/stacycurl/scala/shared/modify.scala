package stacycurl.scala.shared

import scalaz._


object Modify {
  implicit object ModifyInstance extends InvariantFunctor[Modify] with Zip[Modify] {
    def xmap[A, B](ma: Modify[A], aToB: A => B, bToA: B => A): Modify[B] = ma.xmap[B](aToB, bToA)

    def zip[A, B](ma: => Modify[A], mb: => Modify[B]): Modify[(A, B)] =
      Modify[(A, B)] { case (a, b) => (ma(a), mb(b)) }
  }
}

case class Modify[A](f: A => A) extends (A => A) {
  def apply(a: A) = f(a)
  def xmap[B](aToB: A => B, bToA: B => A): Modify[B] = Modify[B](bToA andThen this andThen aToB)
  def lens[B](lens: Lens[B, A]): Modify[B] = Modify[B](lens.mod(this, _))

  def zip[B](mb: Modify[B]): Modify[(A, B)] = Modify[(A, B)] {
    case (a, b) => (f(a), mb.f(b))
  }
}

object Update {
  def apply[A](action: A => Unit): Modify[A] = new Modify[A]((a: A) => {action(a); a})
}
