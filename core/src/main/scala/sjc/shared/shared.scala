package sjc.shared

import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.generic._
import scalaz._


object Shared {
  def apply[A](initial: A, lock: Lock = Synchronized(new Object)): Shared[A] = LockShared[A](initial, lock)
  def threadLocal[A](initial: A, lock: Lock = Unlocked): Shared[A] = ThreadLocalShared[A](initial, lock)

  implicit object SharedInstance extends InvariantFunctor[Shared] with Unzip[Shared] with Zip[Shared] {
    def xmap[A, B](sa: Shared[A], aToB: A => B, bToA: B => A): Shared[B] = sa.xmap(aToB, bToA)
    def zip[A, B](sa: => Shared[A], sb: => Shared[B]): Shared[(A, B)] = sa.zip(sb)

    def unzip[A, B](sab: Shared[(A, B)]): (Shared[A], Shared[B]) =
      (sab.lens(Lens.firstLens), sab.lens(Lens.secondLens))
  }

  implicit def sharedShow[A: Show]: Show[Shared[A]] =
    Show.show[Shared[A]]((sa: Shared[A]) => Show[A].show(sa.get()))

  implicit class SharedOps[A](sa: Shared[A]) {
    def +=(a: A)(implicit N: Numeric[A]) = sa.modify(N.plus(_, a))
    def -=(a: A)(implicit N: Numeric[A]) = sa.modify(N.minus(_, a))
    def *=(a: A)(implicit N: Numeric[A]) = sa.modify(N.times(_, a))
    def /=(a: A)(implicit F: Fractional[A]) = sa.modify(F.div(_, a))
    def append(a: A)(implicit S: Semigroup[A]) = sa.modify(S.append(_, a))
    def clear()(implicit M: Monoid[A]): Shared[A] = { sa.value = M.zero; sa }
  }

  implicit class SharedList[A](list: Shared[List[A]]) extends Builder[A, List[A]] {
    def +=(a: A): this.type = { list.modify(_ :+ a); this }
    def clear(): Unit       = list.value = Nil
    def result(): List[A]   = list.get()
    def drain(): List[A]    = { val result = this.result(); clear(); result }
  }

  implicit class SharedMap[K, V](map: Shared[Map[K, V]]) extends Builder[(K, V), Map[K, V]] {
    def +=(kv: (K, V))      = { map.modify(_ + kv); this }
    def clear(): Unit       = map.value = Map.empty[K, V]
    def result(): Map[K, V] = map.get()
  }

  implicit class SharedSeqLike[A, Repr, CC[A] <: SeqLike[A, CC[A]]](seqLike: Shared[CC[A]]) {
    def sortBy[B: scala.Ordering](f: A => B): Shared[CC[A]] = seqLike.transform(_.sortBy(f))
    def sortWith(lt: (A, A) => Boolean): Shared[CC[A]] = seqLike.transform(_.sortWith(lt))
    def sorted[B >: A : scala.Ordering]: Shared[CC[A]] = seqLike.transform(_.sorted[B])
  }
}

trait Shared[A] extends Reader[A] {
  def await(p: A => Boolean, timeoutMillis: Long = 0): Option[A] = {
    val (start, minSleep) = (System.currentTimeMillis(), 100)

    @tailrec def recurse(value: A): Option[A] = {
      val remainingTime =
        if (timeoutMillis == 0) minSleep else timeoutMillis - (System.currentTimeMillis() - start)

      if (p(value)) Some(value) else if (remainingTime <= 0) None else {
        Thread.sleep(math.min(minSleep, remainingTime))
        recurse(get())
      }
    }

    recurse(get())
  }

  def changes(p: Change[A] => Boolean = null): Changes[A] = {
    val result = Shared[List[Change[A]]](Nil)

    onChange(result += _)

    val changes = Changes[A](result)

    if (p == null) changes else changes.filter(p)
  }

  def onChange(callback: Change[A] => Unit): this.type = onChange(Callback(callback))
  def onChange(callback: Callback[A]): this.type

  def value: A = get()
  def value_=(a: A): Change[A] = set(a)
  def set(a: A): Change[A] = modify(_ => a)

  def withValue[B](a: A)(action: => B): B = {
    val change = set(a)

    try { action } finally { set(change.before) }
  }

  def update(action: A => Unit): Change[A] = alter((a: A) => {action(a); new Unchanged(a)})
  def modify(f: A => A): Change[A] = alter((a: A) => Change(a, f(a)))
  def alter(f: A => Change[A]): Change[A]

  def lens[B](lens: Lens[A, B]): Shared[B]           = LensShared[A, B](this, lens)
  def transform(f: A => A): Shared[A]                = xmap[A](f, identity[A])
  def xmap[B](aToB: A => B, bToA: B => A): Shared[B] = XMapShared[A, B](this, aToB, bToA)
  def zip[B](sb: Shared[B]): Shared[(A, B)]          = ZippedShared[A, B](this, sb)
  def filter(p: Change[A] => Boolean): Shared[A]     = FilteredShared[A](this, p)

  def lock: Lock
}

case class LockShared[A](initial: A, lock: Lock) extends Shared[A] {
  def get(): A = lock.withRead(current)
  def onChange(callback: Callback[A]): this.type = {callbacks += callback; this}

  def alter(f: A => Change[A]): Change[A] =
    callbacks.apply(lock.withWrite(f(value).perform(change => current = change.after)))

  private var current = initial
  private val callbacks: Callbacks[A] = new Callbacks[A]
}

case class ThreadLocalShared[A](initial: A, lock: Lock = Unlocked) extends Shared[A] {
  def get(): A = dynamic.value
  def onChange(callback: Callback[A]): this.type = {callbacks += callback; this}

  def alter(f: A => Change[A]): Change[A] =
    callbacks.apply(lock.withWrite(f(value).perform(change => dynamic.value = change.after)))

  private val dynamic = new scala.util.DynamicVariable[A](initial)
  private val callbacks: Callbacks[A] = new Callbacks[A]
}

case class XMapShared[A, B](sa: Shared[A], aToB: A => B, bToA: B => A) extends Shared[B] {
  def get(): B = aToB(sa.get())
  def onChange(callbackB: Callback[B]): this.type = {sa.onChange(callbackB.contramap(aToB)); this}
  def alter(f: B => Change[B]): Change[B] = sa.alter((a: A) => f(aToB(a)).map(bToA)).map(aToB)
  def lock = sa.lock
}

case class LensShared[A, B](sa: Shared[A], lens: Lens[A, B]) extends Shared[B] {
  def get(): B = lens.get(sa.get())
  def onChange(callbackB: Callback[B]): this.type = {sa.onChange(callbackB.contramap(lens.get)); this}
  def alter(f: B => Change[B]): Change[B] = sa.alter((a: A) => lens.modf(f, a)).map(lens.get)
  def lock = sa.lock
}

case class FilteredShared[A](sa: Shared[A], p: Change[A] => Boolean) extends Shared[A] {
  def get(): A = sa.get()
  def onChange(callback: Callback[A]): this.type = { sa.onChange(callback); this }
  def alter(f: A => Change[A]): Change[A] = sa.alter((a: A) => f(a).filter(p))
  def lock: Lock = sa.lock
}

case class ZippedShared[A, B](sa: Shared[A], sb: Shared[B]) extends Shared[(A, B)] {
  def get(): (A, B) = (sa.get(), sb.get())

  def onChange(callbackAB: Callback[(A, B)]): this.type = {
    callbacks += callbackAB
    val guardedCallback = callbackAB.guard(allowCallback)
    sa.onChange((changeA: Change[A]) => guardedCallback(changeA.zip(sb.unchanged())))
    sb.onChange((changeB: Change[B]) => guardedCallback(sa.unchanged().zip(changeB)))
    this
  }

  def alter(f: ((A, B)) => Change[(A, B)]): Change[(A, B)] = callbacks.apply(lock.withWrite {
    f((sa.get(), sb.get())).perform(change => allowCallback.withValue(false) {
      sa.value = change.after._1; sb.value = change.after._2
    })
  })

  val lock = new ZippedLock(sa.lock, sb.lock)

  private val allowCallback: Shared[Boolean] = Shared(true)
  private val callbacks: Callbacks[(A, B)] = new Callbacks[(A, B)]
}
