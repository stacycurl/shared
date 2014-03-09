package stacycurl.scala.shared

import java.util.concurrent.locks.{ Lock => JLock, ReadWriteLock => JRWLock, ReentrantReadWriteLock => JRRWLock }
import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.generic._
import scalaz._


object Shared {
  def apply[A](initial: A, lock: Lock = Synchronized(new Object)): Shared[A] = LockShared[A](initial, lock)

  implicit object SharedInvariantFunctor extends InvariantFunctor[Shared] {
    def xmap[A, B](sa: Shared[A], aToB: A => B, bToA: B => A): Shared[B] = sa.xmap(aToB, bToA)
  }

  implicit class SharedList[A](list: Shared[List[A]]) extends Builder[A, List[A]] {
    def +=(a: A): this.type = { list.modify(_ :+ a); this }
    def clear(): Unit       = list.modify(_ => Nil)
    def result(): List[A]   = list.get()
  }

  implicit class SharedMap[K, V](map: Shared[Map[K, V]]) {
    def +=(kv: (K, V)) = map.modify(_ + kv)
  }

  implicit class SharedSeqLike[A, Repr, CC[A] <: SeqLike[A, CC[A]]](seqLike: Shared[CC[A]]) {
    def sortBy[B](f: A => B)(implicit ordering: scala.Ordering[B]): Shared[CC[A]] = seqLike.transform(_.sortBy(f))
    def sortWith(lt: (A, A) => Boolean): Shared[CC[A]] = seqLike.transform(_.sortWith(lt))
    def sorted[B >: A](implicit ordering: scala.Ordering[B]): Shared[CC[A]] = seqLike.transform(_.sorted[B])
  }

  implicit object SharedZip extends Zip[Shared] {
    def zip[A, B](sa: => Shared[A], sb: => Shared[B]): Shared[(A, B)] = sa.zip(sb)
  }
}

trait Shared[A] extends Reader[A] {
  def await(p: A => Boolean, timeoutMillis: Long = 0): Option[A] = {
    val (start, minSleep) = (System.currentTimeMillis(), 100)

    @tailrec def recurse(): Option[A] = {
      val value = get()

      val remainingTime =
        if (timeoutMillis == 0) minSleep else timeoutMillis - (System.currentTimeMillis() - start)

      if (p(value)) Some(value) else if (remainingTime <= 0) None else {
        Thread.sleep(math.min(minSleep, remainingTime))
        recurse()
      }
    }

    recurse()
  }

  def changes: Reader[List[Change[A]]] = {
    val result = Shared[List[Change[A]]](Nil)

    onChange(result += _)

    result
  }

  def onChange(callback: Change[A] => Unit): this.type = onChange(Callback(callback))
  def onChange(callback: Callback[A]): this.type

  def modify(f: A => A): Change[A]

  def lens[B](lens: Lens[A, B]): Shared[B]           = LensShared[A, B](this, lens)
  def transform(f: A => A): Shared[A]                = xmap[A](f, identity[A])
  def xmap[B](aToB: A => B, bToA: B => A): Shared[B] = XMapShared[A, B](this, aToB, bToA)
  def zip[B](sb: Shared[B]): Shared[(A, B)]          = ZippedShared[A, B](this, sb)

  def lock: Lock
}

case class LockShared[A](initial: A, lock: Lock) extends Shared[A] {
  private var value = initial

  def get(): A = lock.withRead(value)
  def onChange(callback: Callback[A]): this.type = {callbacks += callback; this}

  def modify(f: A => A): Change[A] = {
    val change = lock.withWrite {
      val oldA = value
      value = f(oldA)

      Change(oldA, value)
    }

    callbacks.foreach(callback => callback(change))
    change
  }

  private lazy val callbacks: Shared[List[Callback[A]]] = Shared(Nil)
}

object Callback {
  implicit object CallbackContravariant extends Contravariant[Callback] {
    def contramap[A, B](ca: Callback[A])(f: B => A): Callback[B] = ca.contramap(f)
  }
}

case class Callback[A](value: Change[A] => Unit) extends (Change[A] => Unit) {
  def apply(change: Change[A]): Unit = value(change)

  def contramap[B](bToA: B => A): Callback[B] =
    Callback[B]((changeB: Change[B]) => apply(changeB.map(bToA)))

  def guard(condition: Shared[Boolean]): Callback[A] =
    Callback[A]((changeA: Change[A]) => if (condition.get()) apply(changeA))
}

case class XMapShared[A, B](sa: Shared[A], aToB: A => B, bToA: B => A) extends Shared[B] {
  def get(): B = aToB(sa.get())
  def onChange(callbackB: Callback[B]): this.type = {sa.onChange(callbackB.contramap(aToB)); this}
  def modify(f: B => B): Change[B] = sa.modify(aToB andThen f andThen bToA).map(aToB)
  def lock = sa.lock
}

case class LensShared[A, B](sa: Shared[A], lens: Lens[A, B]) extends Shared[B] {
  def get(): B = lens.get(sa.get())
  def onChange(callbackB: Callback[B]): this.type = {sa.onChange(callbackB.contramap(lens.get)); this}
  def modify(f: B => B): Change[B] = sa.modify(lens.mod(f, _)).map(lens.get)
  def lock = sa.lock
}

case class ZippedShared[A, B](sa: Shared[A], sb: Shared[B]) extends Shared[(A, B)] {
  def get(): (A, B) = (sa.get(), sb.get())

  def onChange(callbackAB: Callback[(A, B)]): this.type = {
    callbacks += callbackAB
    val guardedCallback = callbackAB.guard(allowCallback)
    sa.onChange((changeA: Change[A]) => guardedCallback(changeA.zip(Change.pure(sb.get()))))
    sb.onChange((changeB: Change[B]) => guardedCallback(Change.pure(sa.get()).zip(changeB)))
    this
  }

  def modify(f: ((A, B)) => (A, B)): Change[(A, B)] = {
    val change = lock.withWrite {
      val oldAB@(oldA, oldB) = (sa.get(), sb.get())
      val newAB@(newA, newB) = f(oldA, oldB)

      allowCallback.modify(_ => false)
      sa.modify(_ => newA)
      sb.modify(_ => newB)
      allowCallback.modify(_ => true)

      Change(oldAB, newAB)
    }

    callbacks.foreach(callback => callback(change))
    change
  }

  val lock = new ZippedLock(sa.lock, sb.lock)

  private lazy val allowCallback: Shared[Boolean] = Shared(true)
  private lazy val callbacks: Shared[List[Callback[(A, B)]]] = Shared(Nil)
}

object Lock {
  // Bit odd, since Lock has no type parameter, gonna see what happens though
  implicit object lockZip extends Zip[({type L[A] = Lock})#L] {
    def zip[A, B](la: => Lock, lb: => Lock) = la.zip(lb)
  }
}

trait Lock {
  def withRead[A](f: => A): A
  def withWrite[A](f: => A): A
  def zip(other: Lock): Lock = new ZippedLock(this, other)
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

class ZippedLock(left: Lock, right: Lock) extends Lock {
  def withRead[A](f: => A): A  = left.withRead(right.withRead(f))
  def withWrite[A](f: => A): A = left.withWrite(right.withWrite(f))
}

object Reader {
  implicit def readerAsA[A](ra: Reader[A]): A = ra.get()

  implicit object ReaderFunctor extends Functor[Reader] {
    def map[A, B](ra: Reader[A])(f: A => B): Reader[B] = ra.map(f)
  }

  implicit object readerZip extends Zip[Reader] {
    def zip[A, B](ra: => Reader[A], rb: => Reader[B]): Reader[(A, B)] = ra.zip(rb)
  }
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

object Change {
  def pure[A](a: A): Change[A] = Change[A](a, a)
  def many[A](as: A*): List[Change[A]] = as.zip(as.tail).map(tuple).toList
  def tuple[A](beforeAfter: (A, A)): Change[A] = Change(beforeAfter._1, beforeAfter._2)

  implicit object changeFunctor extends Functor[Change] {
    def map[A, B](ca: Change[A])(f: A => B): Change[B] = ca.map(f)
  }

  implicit object changeZip extends Zip[Change] {
    def zip[A, B](ca: => Change[A], cb: => Change[B]): Change[(A, B)] = ca.zip(cb)
  }
}

case class Change[+A](before: A, after: A) {
  def calc[B](g: (A, A) => B) = g(before, after)
  def map[B](f: A => B): Change[B] = Change[B](f(before), f(after))
  def zip[B](cb: Change[B]): Change[(A, B)] = Change((before, cb.before), (after, cb.after))
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

  def zip[B](mb: Modify[B]): Modify[(A, B)] = Modify[(A, B)] {
    case (a, b) => (f(a), mb.f(b))
  }
}
