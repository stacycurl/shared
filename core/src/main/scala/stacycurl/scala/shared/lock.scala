package stacycurl.scala.shared

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

case object Unlocked extends Lock {
  def withRead[A](f: => A): A = f
  def withWrite[A](f: => A): A = f
  override def zip(other: Lock): Lock = other
}

class ZippedLock(left: Lock, right: Lock) extends Lock {
  def withRead[A](f: => A): A  = left.withRead(right.withRead(f))
  def withWrite[A](f: => A): A = left.withWrite(right.withWrite(f))
}

// Not sure if this is useful at all, just 'closing' over the types defined
class SharedLock(value: Shared[Lock] = Shared(Synchronized())) extends Lock with Shared[Lock] {
  def withRead[A](f: => A): A = value.get().withRead[A](f)
  def withWrite[A](f: => A): A = value.get().withWrite[A](f)

  def get(): Lock = value.get()
  def lock: Lock = value.lock
  def modify(f: Lock => Lock): Change[Lock] = value.modify(f)
  def onChange(callback: Callback[Lock]) = { value.onChange(callback); this }
}
