package sjc.shared

import java.util.concurrent.locks.{ Lock => JLock, ReadWriteLock => JRWLock, ReentrantReadWriteLock => JRRWLock }
import scalaz._


trait Lock {
  def withRead[A](f: => A): A
  def withWrite[A](f: => A): A
  def zip(other: Lock): Lock = ZippedLock(this, other)
  def tap(callback: LockEvent => Unit): Lock = TappedLock(this, callback)

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

case class ZippedLock(left: Lock, right: Lock) extends Lock {
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

case class TappedLock(lock: Lock, callback: LockEvent => Unit) extends Lock {
  def withRead[A](f: => A): A = {
    callback(EnteringRead(lock))
    try lock.withRead(f) finally callback(LeavingRead(lock))
  }

  def withWrite[A](f: => A): A = {
    callback(EnteringWrite(lock))
    try lock.withWrite(f) finally callback(LeavingWrite(lock))
  }

  private[shared] val isStable = lock.isStable
  private[shared] def identity = lock.identity
  private[shared] def locks = if (isStable) stableLocks else tapLocks

  private lazy val stableLocks = tapLocks
  private def tapLocks = lock.locks.map(_.tap(callback))
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

sealed trait LockEvent
case class EnteringRead(lock: Lock) extends LockEvent
case class LeavingRead(lock: Lock) extends LockEvent
case class EnteringWrite(lock: Lock) extends LockEvent
case class LeavingWrite(lock: Lock) extends LockEvent
