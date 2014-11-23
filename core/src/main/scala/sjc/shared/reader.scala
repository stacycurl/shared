package sjc.shared

import scala.language.implicitConversions


object Reader {
  implicit def readerAsA[A](ra: Reader[A]): A = ra.get()
}

trait Reader[+A] extends (() => A) {
  def apply(): A = get()
  def unchanged(): Change[A] = new Unchanged[A](get())
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
