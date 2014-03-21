package stacycurl.scala.shared

import scalaz._


object Change {
  def point[A](a: A): Change[A] = Change[A](a, a)
  def many[A](as: A*): List[Change[A]] = if (as.isEmpty) Nil else as.zip(as.tail).map(tuple).toList
  def tuple[A](beforeAfter: (A, A)): Change[A] = Change(beforeAfter._1, beforeAfter._2)

  implicit object ChangeInstance extends Comonad[Change] with Monad[Change]
    with Traverse[Change] with Unzip[Change] with Zip[Change] {

    def point[A](a: => A): Change[A] = Change(a, a)
    def copoint[A](ca: Change[A]): A = ca.after

    def bind[A, B](ca: Change[A])(f: A => Change[B]): Change[B] = ca.flatMap(f)
    def cobind[A, B](ca: Change[A])(f: Change[A] => B): Change[B] = point(f(ca))

    def traverseImpl[G[_]: Applicative, A, B](ca: Change[A])(f: A => G[B]): G[Change[B]] =
      Functor[G].map[B, Change[B]](f(ca.after))((b: B) => point(b))

    def zip[A, B](ca: => Change[A], cb: => Change[B]): Change[(A, B)] = ca.zip(cb)

    def unzip[A, B](cab: Change[(A, B)]): (Change[A], Change[B]) = (cab.map(_._1), cab.map(_._2))

    override def map[A, B](ca: Change[A])(f: A => B): Change[B] = ca.map(f)
  }

  implicit class ChangeOps[A](change: Change[A]) {
    // Can't define this in Change due its contravariance
    def join(other: Change[A]): Change[A] = Change[A](change.before, other.after)
    def delta(implicit ev: A =:= Int): Int = change.after - change.before
  }
}

case class Change[+A](before: A, after: A) {
  def calc[B](g: (A, A) => B) = g(before, after)
  def map[B](f: A => B): Change[B] = Change[B](f(before), f(after))
  def flatMap[B](f: A => Change[B]): Change[B] = f(after)
  def zip[B](cb: Change[B]): Change[(A, B)] = Change((before, cb.before), (after, cb.after))
  def filter(p: Change[A] => Boolean): Change[A] = if (p(this)) this else Change(before, before)
}
