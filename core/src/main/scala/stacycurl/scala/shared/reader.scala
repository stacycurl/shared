package stacycurl.scala.shared

import scalaz._


object Reader {
  implicit def readerAsA[A](ra: Reader[A]): A = ra.get()

  implicit object ReaderInstance extends Comonad[Reader] with Cozip[Reader] with Monad[Reader]
    with Traverse[Reader] with Zip[Reader] {

    def point[A](a: => A): Reader[A] = FunctionReader[A](() => a)
    def copoint[A](ra: Reader[A]): A = ra.get()

    def bind[A, B](ra: Reader[A])(f: A => Reader[B]): Reader[B] = point(f(ra.get()).get())
    def cobind[A, B](ra: Reader[A])(f: Reader[A] => B): Reader[B] = point(f(ra))

    def traverseImpl[G[_]: Applicative, A, B](ra: Reader[A])(f: A => G[B]): G[Reader[B]] =
      Functor[G].map[B, Reader[B]](f(ra.get()))((b: B) => point(b))

    def zip[A, B](ra: => Reader[A], rb: => Reader[B]): Reader[(A, B)] = ra.zip(rb)

    def cozip[A, B](rab: Reader[A \/ B]): Reader[A] \/ Reader[B] =
      rab.get().bimap((a: A) => point(a), (b: B) => point(b))

    override def map[A, B](ra: Reader[A])(f: A => B): Reader[B] = ra.map(f)
  }

  implicit def readerShow[A: Show]: Show[Reader[A]] =
    Show.show[Reader[A]]((ra: Reader[A]) => Show[A].show(ra.get()))
}

trait Reader[+A] {
  def get(): A
  def map[B](f: A => B): Reader[B] = MappedReader(this, f)
  def zip[B](rb: Reader[B]): Reader[(A, B)] = ZippedReader[A, B](this, rb)
}

case class MappedReader[A, +B](ra: Reader[A], f: A => B) extends Reader[B] {
  def get(): B = f(ra.get())
}

case class ZippedReader[+A, +B](ra: Reader[A], rb: Reader[B]) extends Reader[(A, B)] {
  def get(): (A, B) = (ra.get(), rb.get())
}

case class FunctionReader[A](f: () => A) extends Reader[A] {
  def get(): A = f()
}
