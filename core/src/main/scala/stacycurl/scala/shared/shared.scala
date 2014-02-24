package stacycurl.scala.shared

import java.util.concurrent.locks.{ Lock => JLock, ReadWriteLock => JRWLock, ReentrantReadWriteLock => JRRWLock }
import scala.collection._
import scalaz._


object Shared {
  def apply[A](initial: A, lock: Lock = Synchronized(new Object)): Shared[A] = LockShared[A](initial, lock)

  implicit object SharedInvariantFunctor extends InvariantFunctor[Shared] {
    def xmap[A, B](sa: Shared[A], aToB: A => B, bToA: B => A): Shared[B] =
      sa.xmap(aToB, bToA)
  }

  implicit class SharedList[A](list: Shared[List[A]]) {
    def +=(a: A) = list.modify(_ ++ List(a))
    def clear()  = list.modify(_ => Nil)
  }

  implicit class SharedSeqLike[A, Repr, CC[A] <: SeqLike[A, CC[A]]](seqLike: Shared[CC[A]]) {
    def sortBy[B](f: A => B)(implicit ordering: scala.Ordering[B]): Shared[CC[A]] =
      seqLike.transform(_.sortBy(f))

    def sortWith(lt: (A, A) => Boolean): Shared[CC[A]] =
      seqLike.transform(_.sortWith(lt))

    def sorted[B >: A](implicit ordering: scala.Ordering[B]): Shared[CC[A]] =
      seqLike.transform(_.sorted[B])
  }
}

trait Shared[A] {
  def get(): A
  def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B

  def modify(f: A => A): A               = modifyAndCalc(f) { case (old, _)      => old      }
  def modifyAndGet(f: A => A): A         = modifyAndCalc(f) { case (_, modified) => modified }
  def modify[B](update: Update[A, B]): B = update(this)
  def reader: Reader[A]                  = SharedReader[A](this)

  def lens[B](lens: Lens[A, B]): Shared[B]           = LensShared[A, B](this, lens)
  def transform(f: A => A): Shared[A]                = xmap[A](f, identity[A])
  def xmap[B](aToB: A => B, bToA: B => A): Shared[B] = XMapShared[A, B](this, aToB, bToA)
}

case class LockShared[A](initial: A, lock: Lock) extends Shared[A] {
  private var value = initial

  def get(): A = lock.withRead(value)

  def modifyAndCalc[B](f: A => A)(g: (A, A) => B): B = {
    val (oldA, newA) = lock.withWrite {
      val oldA = value
      value = f(oldA)

      (oldA, value)
    }

    g(oldA, newA)
  }
}

case class XMapShared[A, B](sa: Shared[A], aToB: A => B, bToA: B => A) extends Shared[B] {
  def get(): B = aToB(sa.get())

  def modifyAndCalc[C](f: B => B)(g: (B, B) => C): C = {
    sa.modifyAndCalc(aToB andThen f andThen bToA) {
      case (oldA, newA) => g(aToB(oldA), aToB(newA))
    }
  }
}

case class LensShared[A, B](sa: Shared[A], lens: Lens[A, B]) extends Shared[B] {
  def get(): B = lens.get(sa.get())

  def modifyAndCalc[C](f: B => B)(g: (B, B) => C): C = {
    sa.modifyAndCalc[C](lens.mod(f, _)) {
      case (oldA, newA) => g(lens.get(oldA), lens.get(newA))
    }
  }
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
  def withRead[A](f: => A): A   = withLock(lock.readLock(), f)
  def withWrite[A](f: => A): A  = withLock(lock.writeLock(), f)

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

case class SharedReader[A](sa: Shared[A]) extends Reader[A] {
  def get(): A = sa.get()
}

case class MappedReader[A, +B](ra: Reader[A], f: A => B) extends Reader[B] {
  def get(): B = f(ra.get())
}

object Update {
  implicit def updateFunctor[A]: Functor[({type b[B] = Update[A, B]})#b] = 
    new Functor[({type b[B] = Update[A, B]})#b] {
      def map[B, C](abu: Update[A, B])(f: B => C): Update[A, C] = abu.map(f)
    }

  implicit def updateInvariantFunctor[B]: InvariantFunctor[({type a[A] = Update[A, B]})#a] =
    new InvariantFunctor[({type a[A] = Update[A, B]})#a] {
      def xmap[A, C](abu: Update[A, B], aToC: A => C, cToA: C => A): Update[C, B] =
        abu.xmap[C](aToC, cToA)
    }
}

trait Update[A, +B] {
  def apply(shared: Shared[A]): B
  def map[C](f: B => C): Update[A, C] = MappedUpdate[A, B, C](this, f)
  def xmap[C](aToC: A => C, cToA: C => A): Update[C, B] = XMappedUpdate[A, B, C](this, aToC, cToA)
  def lens[C](lens: Lens[C, A]): Update[C, B] = LensUpdate[A, B, C](this, lens)
}

case class Modify[A](f: A => A) extends Update[A, A] {
  def apply(shared: Shared[A]): A = shared.modify(f)
  def andGet = ModifyAndGet[A](f)
  def andCalc[B](g: (A, A) => B) = ModifyAndCalc[A, B](f, g)
}

case class ModifyAndGet[A](f: A => A) extends Update[A, A] {
  def apply(shared: Shared[A]): A = shared.modifyAndGet(f)
}

case class ModifyAndCalc[A, +B](f: A => A, g: (A, A) => B) extends Update[A, B] {
  def apply(shared: Shared[A]): B = shared.modifyAndCalc(f)(g)
}

case class MappedUpdate[A, B, +C](abu: Update[A, B], f: B => C) extends Update[A, C] {
  def apply(shared: Shared[A]): C = f(abu.apply(shared))
}

case class XMappedUpdate[A, +B, C](abu: Update[A, B], aToC: A => C, cToA: C => A) extends Update [C, B] {
  def apply(shared: Shared[C]): B = abu.apply(shared.xmap[A](cToA, aToC))
}

case class LensUpdate[A, +B, C](abu: Update[A, B], lens: Lens[C, A]) extends Update[C, B] {
  def apply(shared: Shared[C]): B = abu.apply(shared.lens(lens))
}
