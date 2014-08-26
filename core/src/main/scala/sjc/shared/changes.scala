package sjc.shared

import scalaz._
import scalaz.std.list._


object Changes {
  def many[A](as: A*): Changes[A] = apply(Change.many(as: _*): _*)
  def apply[A](changes: Change[A]*): Changes[A] = apply(Shared(changes.toList))
  def apply[A](changes: Shared[List[Change[A]]]): Changes[A] = SharedChanges(changes)

  implicit object ChangesInstance extends Bind[Changes] with Unzip[Changes] with Zip[Changes] {
    def point[A](a: => A): Changes[A] = SharedChanges(Shared(List(Change.point(a))))

    def bind[A, B](csa: Changes[A])(f: A => Changes[B]): Changes[B] = csa.flatMap(f)

    def unzip[A, B](csab: Changes[(A, B)]): (Changes[A], Changes[B]) =
      (map[(A, B), A](csab)(_._1), map[(A, B), B](csab)(_._2))

    def zip[A, B](csa: => Changes[A], csb: => Changes[B]): Changes[(A, B)] = csa.zip(csb)

    override def map[A, B](csa: Changes[A])(f: A => B): Changes[B] = csa.map(f)
  }

  implicit def equalChanges[A](implicit equalChange: Equal[Change[A]]): Equal[Changes[A]] =
    scalaz.std.list.listEqual[Change[A]].contramap[Changes[A]](_.get())
}

trait Changes[A] extends Reader[List[Change[A]]] {
  def get(): List[Change[A]]
  def clear(): this.type

  def values(): List[A] = get() match {
    case Nil => Nil
    case list@(head :: _) => head.before :: list.map(_.after)
  }

  def map[B](f: A => B): Changes[B] = MappedChanges[A, B](this, f)
  def flatMap[B](f: A => Changes[B]): Changes[B] = FlatMappedChanges[A, B](this, f)
  def zip[B](csb: Changes[B]): Changes[(A, B)] = ZippedChanges[A, B](this, csb)
  def filter(p: Change[A] => Boolean): Changes[A] = FilteredChanges[A](this, p)
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

case class FilteredChanges[A](ca: Changes[A], p: Change[A] => Boolean) extends Changes[A] {
  // maintain continuity, i.e. ca.get()[i].after == ca.get()[i + 1].before
  def get(): List[Change[A]] =  Change.many(ca.values().foldLeft(nil[A]) {
    case (Nil, element)                                              => element :: Nil
    case (previous :: rest, element) if p(Change(previous, element)) => element :: previous :: rest
    case (acc, _)                                                    => acc
  }.reverse: _*)

  def clear(): this.type = { ca.clear(); this }
}
