package stacycurl.scala.shared

import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.generic._
import scalaz._


object Shared {
  def apply[A](initial: A, lock: Lock = Synchronized(new Object)): Shared[A] = LockShared[A](initial, lock)

  implicit object SharedInstance extends InvariantFunctor[Shared] with Zip[Shared] {
    def xmap[A, B](sa: Shared[A], aToB: A => B, bToA: B => A): Shared[B] = sa.xmap(aToB, bToA)

    def zip[A, B](sa: => Shared[A], sb: => Shared[B]): Shared[(A, B)] = sa.zip(sb)
  }

  implicit def sharedShow[A: Show]: Show[Shared[A]] =
    Show.show[Shared[A]]((sa: Shared[A]) => Show[A].show(sa.get()))

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

  def changes: Changes[A] = {
    val result = Shared[List[Change[A]]](Nil)

    onChange(result += _)

    Changes[A](result)
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

  def modify(f: A => A): Change[A] = callbacks(lock.withWrite {
    val oldA = value
    value = f(oldA)

    Change(oldA, value)
  })

  private val callbacks: Callbacks[A] = new Callbacks[A]
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
    sa.onChange((changeA: Change[A]) => guardedCallback(changeA.zip(Change.point(sb.get()))))
    sb.onChange((changeB: Change[B]) => guardedCallback(Change.point(sa.get()).zip(changeB)))
    this
  }

  def modify(f: ((A, B)) => (A, B)): Change[(A, B)] = callbacks(lock.withWrite {
    val oldAB@(oldA, oldB) = (sa.get(), sb.get())
    val newAB@(newA, newB) = f(oldA, oldB)

    allowCallback.modify(_ => false)
    sa.modify(_ => newA)
    sb.modify(_ => newB)
    allowCallback.modify(_ => true)

    Change(oldAB, newAB)
  })

  val lock = new ZippedLock(sa.lock, sb.lock)

  private val allowCallback: Shared[Boolean] = Shared(true)
  private val callbacks: Callbacks[(A, B)] = new Callbacks[(A, B)]
}
