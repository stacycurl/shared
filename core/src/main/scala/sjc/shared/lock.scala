package sjc.shared

import java.util.concurrent.locks.{ Lock => JLock, ReadWriteLock => JRWLock, ReentrantReadWriteLock => JRRWLock }
import scalaz._


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

  private[shared] val isStable: Boolean
  private[shared] def identity: Int
  private[shared] def locks: Stream[Lock]
}

case class Synchronized(monitor: AnyRef = new Object) extends Lock {
  def withRead[A](f: => A): A  = monitor.synchronized(f)
  def withWrite[A](f: => A): A = monitor.synchronized(f)

  private[shared] val isStable = true
  private[shared] val identity = System.identityHashCode(monitor)
  private[shared] val locks = Stream(this)
}

case class ReadWriteLock(lock: JRWLock = new JRRWLock()) extends Lock {
  def withRead[A](f: => A): A  = withLock(lock.readLock(), f)
  def withWrite[A](f: => A): A = withLock(lock.writeLock(), f)

  private def withLock[A](lock: JLock, f: => A): A =
    try { lock.lock(); f } finally { lock.unlock() }

  private[shared] val isStable = true
  private[shared] val identity = System.identityHashCode(lock)
  private[shared] val locks = Stream(this)
}

case object Unlocked extends Lock {
  def withRead[A](f: => A): A = f
  def withWrite[A](f: => A): A = f
  override def zip(other: Lock): Lock = other

  private[shared] val isStable = true
  private[shared] val identity = System.identityHashCode(this)
  private[shared] val locks = Stream(this)
}

class ZippedLock(left: Lock, right: Lock) extends Lock {
  def withRead[A](f: => A): A  = withLocks(f)(_.withRead)
  def withWrite[A](f: => A): A = withLocks(f)(_.withWrite)

  private def withLocks[A](f: => A)(withLock: Lock => (=> A) => A): A = {
    def recurse(locks: Stream[Lock]): A = locks match {
      case head #:: tail => withLock(head)(recurse(tail))
      case _             => f
    }

    recurse(if (isStable) stableLocks else locks)
  }

  private[shared] val isStable = left.isStable && right.isStable
  private[shared] val identity = System.identityHashCode(this)
  private[shared] def locks = (left.locks ++ right.locks).sortBy(_.identity)

  private lazy val stableLocks = locks
}

// Not sure if this is useful at all, just 'closing' over the types defined
class SharedLock(value: Shared[Lock] = Shared(Synchronized())) extends Lock with Shared[Lock] {
  def withRead[A](f: => A): A = value.get().withRead[A](f)
  def withWrite[A](f: => A): A = value.get().withWrite[A](f)

  def get(): Lock = value.get()
  def lock: Lock = value.lock
  def alter(f: Lock => Change[Lock]): Change[Lock] = value.alter(f)
  def onChange(callback: Callback[Lock]) = { value.onChange(callback); this }

  private[shared] val isStable = false
  private[shared] def identity = value.get().identity
  private[shared] def locks = value.get().locks
}
