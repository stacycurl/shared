package stacycurl.scala.shared


object Shared {
  def apply[A](initial: A): Shared[A] = new Shared[A] {
    private var value = initial

    // Strange I haven't needed to add synchronized here
    def get(): A = value

    def modify(f: A => A): A = modifyAndCalc(f) { case (old, _) => old }

    def modifyAndGet(f: A => A): A = modifyAndCalc(f) { case (_, modified) => modified }

    def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B = synchronized {
      val current = value
      value = f(value)

      g(current, value)
    }

    def modify[B](update: Update[A, B]): B = {
      update(this)
    }
  }
}

trait Shared[A] {
  def get(): A
  def modify(f: A => A): A
  def modifyAndGet(f: A => A): A
  def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B
  def modify[B](update: Update[A, B]): B
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
