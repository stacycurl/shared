package stacycurl.scala.shared


object Shared {
  def apply[A](initial: A): Shared[A] = new Shared[A] {
    private var value = initial

    def get(): A = value

    def modify(f: A => A): A = synchronized {
      val current = value

      value = f(value)

      current
    }

    def modifyAndGet(f: A => A): A = synchronized {
      value = f(value)
      value
    }

    def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B = synchronized {
      val current = value
      value = f(value)

      g(current, value)
    }
  }
}

trait Shared[A] {
  def get(): A
  def modify(f: A => A): A
  def modifyAndGet(f: A => A): A
  def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B
}
