package sjc.shared

import scalaz._


object Change {
  def point[A](a: A): Change[A] = new Unchanged[A](a)
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
    def delta(implicit ev: A =:= Int): Int = change.after - change.before

    def join(other: Change[A]): Change[A] =
      change.fold[Change[A]](ua => other)(ca => Change[A](ca.before, other.after))

    def map[B](f: A => B): Change[B] =
      change.fold[Change[B]](ua => new Unchanged(f(ua.value)))(ca => Change[B](f(ca.before), f(ca.after)))

    def revert(sa: Shared[A]): Change[A] = ifChanged(ca => sa.set(ca.before))
    def filter(p: Change[A] => Boolean): Change[A] = ifChanged(ca => if (p(ca)) ca else new Unchanged(ca.before))
    def perform(action: Change[A] => Unit): Change[A] = ifChanged(ca => { action(ca); ca })
    def ifChanged(f: Change[A] => Change[A]): Change[A] = change.fold[Change[A]](ua => ua)(f)
  }
}

case class Change[+A](before: A, after: A) {
  def calc[B](g: (A, A) => B) = g(before, after)
  def flatMap[B](f: A => Change[B]): Change[B] = f(after)
  def zip[B](cb: Change[B]): Change[(A, B)] = Change((before, cb.before), (after, cb.after))
  def fold[B](unchanged: Unchanged[A] => B)(changed: Change[A] => B): B = changed(this)
}

class Unchanged[+A](val value: A) extends Change[A](value, value){
  override def fold[B](unchanged: Unchanged[A] => B)(changed: Change[A] => B): B = unchanged(this)
}
