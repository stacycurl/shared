package stacycurl.scala.shared

import java.util.concurrent.locks.{ Lock => JLock, ReadWriteLock => JRWLock, ReentrantReadWriteLock => JRRWLock }
import scala.collection._
import scala.collection.generic._
import scalaz._


object Shared {
  def apply[A](initial: A, lock: Lock = Synchronized(new Object)): Shared[A] = LockShared[A](initial, lock)

  implicit object SharedInvariantFunctor extends InvariantFunctor[Shared] {
    def xmap[A, B](sa: Shared[A], aToB: A => B, bToA: B => A): Shared[B] = sa.xmap(aToB, bToA)
  }

  implicit class SharedList[A](list: Shared[List[A]]) extends mutable.Builder[A, List[A]] {
    def +=(a: A): this.type = { list.modify(_ :+ a); this }
    def clear(): Unit       = list.modify(_ => Nil)
    def result(): List[A]   = list.get()
  }

  implicit class SharedSeqLike[A, Repr, CC[A] <: SeqLike[A, CC[A]]](seqLike: Shared[CC[A]]) {
    def sortBy[B](f: A => B)(implicit ordering: scala.Ordering[B]): Shared[CC[A]] = seqLike.transform(_.sortBy(f))
    def sortWith(lt: (A, A) => Boolean): Shared[CC[A]] = seqLike.transform(_.sortWith(lt))
    def sorted[B >: A](implicit ordering: scala.Ordering[B]): Shared[CC[A]] = seqLike.transform(_.sorted[B])
  }
}

trait Shared[A] extends Reader[A] {
  def modify(f: A => A): Change[A]

  def lens[B](lens: Lens[A, B]): Shared[B]           = LensShared[A, B](this, lens)
  def transform(f: A => A): Shared[A]                = xmap[A](f, identity[A])
  def xmap[B](aToB: A => B, bToA: B => A): Shared[B] = XMapShared[A, B](this, aToB, bToA)
}

case class LockShared[A](initial: A, lock: Lock) extends Shared[A] {
  private var value = initial

  def get(): A = lock.withRead(value)

  def modify(f: A => A): Change[A] = lock.withWrite {
    val oldA = value
    value = f(oldA)

    Change(oldA, value)
  }
}

case class XMapShared[A, B](sa: Shared[A], aToB: A => B, bToA: B => A) extends Shared[B] {
  def get(): B = aToB(sa.get())
  def modify(f: B => B): Change[B] = sa.modify(aToB andThen f andThen bToA).map(aToB)
}

case class LensShared[A, B](sa: Shared[A], lens: Lens[A, B]) extends Shared[B] {
  def get(): B = lens.get(sa.get())
  def modify(f: B => B): Change[B] = sa.modify(lens.mod(f, _)).map(lens.get)
}

trait Lock {
  def withRead[A](f: => A): A
  def withWrite[A](f: => A): A
}

case class Synchronized(monitor: AnyRef = new Object) extends Lock {
  def withRead[A](f: => A): A  = monitor.synchronized(f)
  def withWrite[A](f: => A): A = monitor.synchronized(f)
}

case class ReadWriteLock(lock: JRWLock = new JRRWLock()) extends Lock {
  def withRead[A](f: => A): A  = withLock(lock.readLock(), f)
  def withWrite[A](f: => A): A = withLock(lock.writeLock(), f)

  private def withLock[A](lock: JLock, f: => A): A = try {
    lock.lock(); f
  }
  finally {
    lock.unlock()
  }
}

object Reader {
  implicit object ReaderFunctor extends Functor[Reader] {
    def map[A, B](ra: Reader[A])(f: A => B): Reader[B] = ra.map(f)
  }
}

trait Reader[+A] {
  def get(): A
  def map[B](f: A => B): Reader[B] = MappedReader(this, f)
}

case class MappedReader[A, +B](ra: Reader[A], f: A => B) extends Reader[B] {
  def get(): B = f(ra.get())
}

object Change {
  implicit object changeFunctor extends Functor[Change] {
    def map[A, B](ca: Change[A])(f: A => B): Change[B] = ca.map(f)
  }
}

case class Change[+A](before: A, after: A) {
  def calc[B](g: (A, A) => B) = g(before, after)
  def map[B](f: A => B): Change[B] = Change[B](f(before), f(after))
}

object Modify {
  implicit def modifyInvariantFunctor[B]: InvariantFunctor[Modify] = new InvariantFunctor[Modify] {
    def xmap[A, B](ma: Modify[A], aToB: A => B, bToA: B => A): Modify[B] = ma.xmap[B](aToB, bToA)
  }
}

case class Modify[A](f: A => A) extends (A => A) {
  def apply(a: A) = f(a)
  def xmap[B](aToB: A => B, bToA: B => A): Modify[B] = Modify[B](bToA andThen this andThen aToB)
  def lens[B](lens: Lens[B, A]): Modify[B] = Modify[B](lens.mod(this, _))
}
