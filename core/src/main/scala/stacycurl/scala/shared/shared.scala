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

    def modify(modify: Modify[A]) {
      modify(this)
    }
  }
}

trait Shared[A] {
  def get(): A
  def modify(f: A => A): A
  def modifyAndGet(f: A => A): A
  def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B
  def modify(modify: Modify[A])
}

case class Modify[A](f: A => A) {
  def apply(shared: Shared[A]) = shared.modify(f)
}
