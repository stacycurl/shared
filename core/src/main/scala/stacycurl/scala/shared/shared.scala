package stacycurl.scala.shared


object Shared {
  def apply[A](a: A): Shared[A] = new Shared[A] {
    def get(): A = a
  }
}

trait Shared[A] {
  def get(): A
}
