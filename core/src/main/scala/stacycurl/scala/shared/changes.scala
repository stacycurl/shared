package stacycurl.scala.shared

import scalaz._
import scalaz.std.list._


object Changes {
  def apply[A](changes: Shared[List[Change[A]]]): Changes[A] = SharedChanges(changes)

  implicit object ChangeeInstance extends Monad[Changes] with Traverse[Changes] with Zip[Changes] {
    def point[A](a: => A): Changes[A] = SharedChanges(Shared(List(Change.point(a))))

    def bind[A, B](csa: Changes[A])(f: A => Changes[B]): Changes[B] = csa.flatMap(f)

    def traverseImpl[G[_]: Applicative, A, B](csa: Changes[A])(f: A => G[B]): G[Changes[B]] = {
      type LC[A] = List[Change[A]]

      val LC: Traverse[LC] = Traverse[List].compose(Traverse[Change])
      val G: Functor[G]    = Functor[G]

      val fgb: LC[G[B]]            = csa.map(f).get()
      val glcb: G[List[Change[B]]] = LC.sequence(fgb)
      val result: G[Changes[B]]    = G.map(glcb)(lcb => Changes(Shared(lcb)))

      result
    }

    def zip[A, B](csa: => Changes[A], csb: => Changes[B]): Changes[(A, B)] = csa.zip(csb)

    override def map[A, B](csa: Changes[A])(f: A => B): Changes[B] = csa.map(f)
  }
}

trait Changes[A] {
  def get(): List[Change[A]]
  def clear(): this.type

  def map[B](f: A => B): Changes[B] = MappedChanges[A, B](this, f)
  def flatMap[B](f: A => Changes[B]): Changes[B] = FlatMappedChanges[A, B](this, f)
  def zip[B](csb: Changes[B]): Changes[(A, B)] = ZippedChanges[A, B](this, csb)
}

case class SharedChanges[A](value: Shared[List[Change[A]]]) extends Changes[A] {
  def get(): List[Change[A]] = value.get()
  def clear(): this.type = { value.clear(); this }
}

case class MappedChanges[A, B](csa: Changes[A], f: A => B) extends Changes[B] {
  def get(): List[Change[B]] = csa.get().map(_.map(f))
  def clear(): this.type = { csa.clear(); this }
}

case class FlatMappedChanges[A, B](csa: Changes[A], f: A => Changes[B]) extends Changes[B] {
  def get(): List[Change[B]] = csa.get().flatMap(ca => f(ca.after).get())
  def clear(): this.type = { csa.clear(); this }
}

case class ZippedChanges[A, B](ca: Changes[A], cb: Changes[B]) extends Changes[(A, B)] {
  def get(): List[Change[(A, B)]] = Zip[List].zipWith(ca.get(), cb.get())(_ zip _)
  def clear(): this.type = { ca.clear(); cb.clear(); this }
}
