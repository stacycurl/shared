package sjc.shared

import scalaz._


object instances {
  object callback {
    implicit object CallbackContravariant extends Contravariant[Callback] {
      def contramap[A, B](ca: Callback[A])(f: B => A): Callback[B] = ca.contramap(f)
    }
  }

  object change {
    implicit object ChangeInstance extends Applicative[Change] with Unzip[Change] with Zip[Change] {
      override def ap[A, B](ca: => Change[A])(cab: => Change[A => B]): Change[B] = {
        val Change(beforeA,  afterA)  = ca
        val Change(beforeAB, afterAB) = cab

        Change[B](beforeAB(beforeA), afterAB(afterA))
      }

      def point[A](a: => A): Change[A] = Change(a, a)

      def zip[A, B](ca: => Change[A], cb: => Change[B]): Change[(A, B)] = ca.zip(cb)

      def unzip[A, B](cab: Change[(A, B)]): (Change[A], Change[B]) = (cab.map(_._1), cab.map(_._2))

      override def map[A, B](ca: Change[A])(f: A => B): Change[B] = ca.map(f)
    }

    implicit def equalChange[A](implicit equal: Equal[A]): Equal[Change[A]] =
      scalaz.std.tuple.tuple2Equal[A, A].contramap[Change[A]](_.tuple)
  }

  object changes {
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

  object modify {
    implicit object ModifyInstance extends InvariantFunctor[Modify] {
      def xmap[A, B](ma: Modify[A], aToB: A => B, bToA: B => A): Modify[B] = ma.xmap[B](aToB, bToA)
    }

    implicit class ModifyOps[A](ma: Modify[A]) {
      def lens[B](lens: Lens[B, A]): Modify[B] = Modify[B](lens.mod(ma, _))
    }
  }

  object reader {
    implicit object ReaderInstance extends Comonad[Reader] with Cozip[Reader] with Monad[Reader]
      with Traverse[Reader] with Unzip[Reader] with Zip[Reader] {

      def point[A](a: => A): Reader[A] = FunctionReader[A](() => a)
      def copoint[A](ra: Reader[A]): A = ra.get()

      def bind[A, B](ra: Reader[A])(f: A => Reader[B]): Reader[B] = point(f(ra.get()).get())
      def cobind[A, B](ra: Reader[A])(f: Reader[A] => B): Reader[B] = point(f(ra))

      def traverseImpl[G[_]: Applicative, A, B](ra: Reader[A])(f: A => G[B]): G[Reader[B]] =
        Functor[G].map[B, Reader[B]](f(ra.get()))((b: B) => point(b))

      def zip[A, B](ra: => Reader[A], rb: => Reader[B]): Reader[(A, B)] = ra.zip(rb)

      def cozip[A, B](rab: Reader[A \/ B]): Reader[A] \/ Reader[B] =
        rab.get().bimap((a: A) => point(a), (b: B) => point(b))

      def unzip[A, B](rab: Reader[(A, B)]): (Reader[A], Reader[B]) = (rab.map(_._1), rab.map(_._2))

      override def map[A, B](ra: Reader[A])(f: A => B): Reader[B] = ra.map(f)
    }

    implicit def equalReader[A: Equal]: Equal[Reader[A]] = Equal.equalBy[Reader[A], A](_.get())

    implicit object ReaderRepresentable extends Representable[Reader, Unit] {
      def rep[A](f: Unit => A): Reader[A] = FunctionReader[A](() => f())
      def unrep[A](ra: Reader[A]): Unit => A = u => ra.get()
    }

    implicit def readerShow[A: Show]: Show[Reader[A]] =
      Show.show[Reader[A]]((ra: Reader[A]) => Show[A].show(ra.get()))

    case class FunctionReader[A](f: () => A) extends Reader[A] {
      def get(): A = f()
    }
  }

  object shared {
    implicit object SharedInstance extends InvariantFunctor[Shared] with Unzip[Shared] {
      def xmap[A, B](sa: Shared[A], aToB: A => B, bToA: B => A): Shared[B] = sa.xmap(aToB, bToA)

      def unzip[A, B](sab: Shared[(A, B)]): (Shared[A], Shared[B]) =
        (sab.lens(Lens.firstLens), sab.lens(Lens.secondLens))
    }

    implicit def sharedEqual[A: Equal]: Equal[Shared[A]] =
      Equal.equalBy[Shared[A], A](_.get())

    implicit def sharedShow[A: Show]: Show[Shared[A]] =
      Show.show[Shared[A]]((sa: Shared[A]) => Show[A].show(sa.get()))

    implicit class SharedExtraOps[A](sa: Shared[A]) {
      def append(a: A)(implicit S: Semigroup[A]) = sa.modify(S.append(_, a))
      def clear()(implicit M: Monoid[A]): Shared[A] = { sa.value = M.zero; sa }
      def lens[B](lens: Lens[A, B]): Shared[B] = LensShared[A, B](sa, lens)
    }

    case class LensShared[A, B](sa: Shared[A], lens: Lens[A, B]) extends Shared[B] {
      def get(): B = lens.get(sa.get())
      def onChange(callbackB: Callback[B]): this.type = {sa.onChange(callbackB.contramap(lens.get)); this}

      def alter(f: B => Change[B]): Change[B] = {
        import change._

        sa.alter((a: A) => lens.modf(f, a)).map(lens.get)
      }

      def lock = sa.lock
    }
  }
}
