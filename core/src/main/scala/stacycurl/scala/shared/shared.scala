package stacycurl.scala.shared


object Shared {
  def apply[A](initial: A): Shared[A] = new Shared[A] {
    private var value = initial

    def get(): A = value

    def modify(f: A => A) {
      value = f(value)
    }
  }
}

trait Shared[A] {
  def get(): A
  def modify(f: A => A)
}
