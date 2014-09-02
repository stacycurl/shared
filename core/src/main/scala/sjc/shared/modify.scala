package sjc.shared



case class Modify[A](f: A => A) extends (A => A) {
  def apply(a: A) = f(a)
  def xmap[B](aToB: A => B, bToA: B => A): Modify[B] = Modify[B](bToA andThen this andThen aToB)

  def zip[B](mb: Modify[B]): Modify[(A, B)] = Modify[(A, B)] {
    case (a, b) => (f(a), mb.f(b))
  }
}

object Update {
  def apply[A](action: A => Unit): Modify[A] = new Modify[A]((a: A) => {action(a); a})
}
