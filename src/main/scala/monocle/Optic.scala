package monocle

import monocle.impl._
import monocle.functions.*
import monocle.internal.*

type Optic[+ThisCan, S, A] = POptic[ThisCan, S, S, A, A]

final class POptic[+ThisCan, -S, +T, +A, -B] private[monocle](protected[monocle] val impl: OpticImpl[ThisCan, S, T, A, B]):

  def andThen[ThatCan >: ThisCan, C, D](o: POptic[ThatCan, A, B, C, D]): POptic[ThatCan, S, T, C, D] =
    POptic(impl.andThen(o.impl))

  def index[I, S1 <: S, A1 >: A, V](i: I)(using evIndex: Index[A1, I, V])(using T <:< S1, A1 <:< B): Optic[ThisCan | (GetOption & Modify), S1, V] = 
    asInstanceOf[Optic[ThisCan, S1, A1]].andThen(evIndex.index(i))

  override def toString() = s"POptic(${impl})"

end POptic

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def all(p: A => Boolean): S => Boolean =
    s => self.impl.safe.foldMap(p)(s)(using Monoids.all)

extension [S, A] (self: Optic[Get, S, A])
  def choice[S1](other: Optic[Get, S1, A]): Optic[Get, Either[S, S1], A] =
    Optic.thatCan.get(_.fold(self.get, other.get))

extension [ThisCan, S, A] (self: Optic[ThisCan, S, A])
  def each[C](using evEach: Each[A, C]): Optic[ThisCan | (GetMany & Modify), S, C] = 
    self.andThen(evEach.each)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def exist(p: A => Boolean): S => Boolean =
    s => self.impl.safe.foldMap(p)(s)(using Monoids.any)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def find(p: A => Boolean): S => Option[A] =
    s => self.impl.safe.foldMap(a => Some(a).filter(p))(s)(using Monoids.firstOption)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def fold(s: S)(using Monoid[A]): A =
    self.impl.safe.foldMap(identity)(s)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def foldMap[M: Monoid](f: A => M)(s: S): M = 
    self.impl.safe.foldMap(f)(s)

extension [S, T, A, B] (self: POptic[Get, S, T, A, B])
  def get(s: S): A = 
    self.impl.safe.get(s)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def getAll(s: S): List[A] = 
    self.impl.safe.foldMap[List[A]](List(_))(s)

extension [S, T, A, B] (self: POptic[GetOneOrMore, S, T, A, B])
  def getOneOrMore(s: S): NonEmptyList[A] = 
    self.impl.safe.nonEmptyFoldMap[NonEmptyList[A]](NonEmptyList(_, Nil))(s)

extension [S, T, A, B] (self: POptic[GetOption, S, T, A, B])
  def getOption(s: S): Option[A] = 
    self.impl.safe.getOption(s)

extension [S, T, A, B] (self: POptic[GetOption & Modify, S, T, A, B])
  def getOrModify(s: S): Either[T, A] = 
    self.impl.safe.getOrModify(s)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def headOption(s: S): Option[A] =
    self.impl.safe.foldMap(Option.apply)(s)(using Monoids.firstOption)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def isEmpty(s: S): Boolean =
    self.impl.safe.foldMap(_ => false)(s)(using Monoids.all)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def lastOption(s: S): Option[A] =
    self.impl.safe.foldMap(Option.apply)(s)(using Monoids.lastOption)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def length(s: S): Int =
    self.impl.safe.foldMap(_ => 1)(s)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
  def nonEmpty(s: S): Boolean =
    !self.isEmpty(s)

extension [S, T, A, B] (self: POptic[GetOneOrMore, S, T, A, B])
  def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = 
    self.impl.safe.nonEmptyFoldMap(f)(s)

extension [S, T, A, B] (self: POptic[GetOneOrMore & Modify, S, T, A, B])
  def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] = 
    self.impl.safe.nonEmptyModifyA(f)(s)

extension [S, T, A, B] (self: POptic[Get & ReverseGet, S, T, A, B])
  def mapping[F[_]: Functor]: POptic[Get & ReverseGet, F[S], F[T], F[A], F[B]] =
    POptic.thatCan.convertBetween[F[S], F[T], F[A], F[B]]
      (fs => Functor[F].map(fs)(self.get))
      (fb => Functor[F].map(fb)(self.reverseGet))

extension [S, T, A, B] (self: POptic[Modify, S, T, A, B])
  def modify(f: A => B): S => T = 
    self.impl.safe.modify(f)

extension [S, T, A, B] (self: POptic[GetMany & Modify, S, T, A, B])
  def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = 
    self.impl.safe.modifyA(f)(s)

extension [S, T, A, B] (self: POptic[Get & Modify, S, T, A, B])
  def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T] = 
    self.impl.safe.modifyF(f)(s)

extension [S, T, A, B] (self: POptic[GetOption & Modify, S, T, A, B])
  def modifyOption(f: A => B): S => Option[T] =
    s => self.impl.safe.getOption(s).map(a => self.impl.safe.replace(f(a))(s))


extension [S, T, A, B] (self: POptic[GetOption & Modify, S, T, A, B])
  def orElse(other: POptic[GetOption & Modify, S, T, A, B]): POptic[GetOption & Modify, S, T, A, B] =
    POptic.thatCan.editOption[S, T, A, B]
      (s => self.getOrModify(s).orElse(other.getOrModify(s)))
      (b => s => self.replaceOption(b)(s).getOrElse(other.replace(b)(s)))

// extension [S, T, A, B] (self: POptic[GetMany & Modify, S, T, A, B])
//   def parModifyF[G[_]](f: A => G[B])(s: S)(using par: Parallel[G]): G[T] =
//     par.sequential(
//       self.modifyA(a => par.parallel(f(a)))(s)(par.applicative)
//     )

// extension [S, T, A, B] (self: POptic[GetOneOrMore & Modify, S, T, A, B])
//   def parNonEmptyModifyF[F[_]](f: A => F[B])(s: S)(using par: NonEmptyParallel[F]): F[T] =
//     par.sequential(
//       self.nonEmptyModifyA(a => par.parallel(f(a)))(s)(par.apply)
//     )

extension [S, T, A, B] (self: POptic[ReverseGet, S, T, A, B])
  def re: Optic[Get, B, T] = 
    Optic.thatCan.get(self.reverseGet)

extension [S, T, A, B] (self: POptic[Modify, S, T, A, B])
  def replace(b: B): S => T = 
    self.impl.safe.modify(_ => b)

extension [S, T, A, B] (self: POptic[GetOption & Modify, S, T, A, B])
  def replaceOption(b: B): S => Option[T] =
    self.modifyOption(_ => b)

extension [S, T, A, B] (self: POptic[ReverseGet, S, T, A, B])
  def reverseGet(b: B): T = 
    self.impl.safe.reverseGet(b)

extension [S, T, A, B] (self: POptic[Get, S, T, A, B])
  def split[S1, A1](other: Optic[Get, S1, A1]): Optic[Get, (S, S1), (A, A1)] =
    Optic.thatCan.get((s, s1) => (self.get(s), other.get(s1)))

extension [ThisCan, S, T, A, B] (self: POptic[ThisCan, S, T, Option[A], Option[B]])
  def some: POptic[ThisCan | GetOption, S, T, A, B] = 
    self.andThen(std.option.pSome)

extension [S, T, A, B] (self: POptic[GetMany, S, T, A, B])
 def to[C](f: A => C) = 
   val g = POptic.thatCan.edit(f)(???)
   self.andThen(g)

extension [ThisCan, S, T, A, B] (self: POptic[ThisCan, S, T, Option[A], Option[B]])
  def withDefault(defaultValue: A): POptic[ThisCan | (Get & ReverseGet), S, T, A, B] = 
    val iso = POptic.thatCan.convertBetween[Option[A], Option[B], A, B](_.getOrElse(defaultValue))(Some.apply)
    self.andThen(iso)

extension [S, A] (self: Optic[Get, S, A])
  def zip[A1](other: Optic[Get, S, A1]): Optic[Get, S, (A, A1)] =
    Optic.thatCan.get(s => (self.get(s), other.get(s)))


// Optional overrides: isEmpty, nonEmpty, find, exist, all
// Lens overrides: modifyA, foldMap, find, exist
// Prism overrides: modifyA, modify, replace
// Iso overrides: foldMap, find, exist, modifyF, modifyA, modify, replace

object POptic:
  def id[A, B]: POptic[Get & ReverseGet, A, B, A, B] = 
    thatCan.convertBetween[A, B, A, B](identity)(identity)

  object thatCan:
    def modify[S, T, A, B](_modify: (A => B) => S => T): POptic[Modify, S, T, A, B] = 
      POptic(
        new SetterImpl:
          override def modify(f: A => B) = _modify(f)
          override def replace(b: B): S => T = _modify(_ => b))

    def edit[S, T, A, B](_get: S => A)(_replace: B => S => T): POptic[Get & Modify, S, T, A, B] = 
      POptic(
        new LensImpl:
          override def get(s: S): A = _get(s)
          override def replace(b: B): S => T = _replace(b)
          override def modifyF[F[+_]](f: A => F[B])(s: S)(using fun: Functor[F]): F[T] = fun.map(f(_get(s)))(a => _replace(a)(s))
          override def modify(f: A => B): S => T = s => _replace(f(_get(s)))(s))

    def editOption[S, T, A, B](_getOrModify: S => Either[T, A])(_replace: B => S => T): POptic[GetOption & Modify, S, T, A, B] = 
      POptic(
        new OptionalImpl:
          override def getOrModify(s: S): Either[T, A] = _getOrModify(s)
          override def replace(b: B): S => T = _replace(b)
          override def getOption(s: S): Option[A] = _getOrModify(s).toOption)

    def edit2[S, T, A, B](_get1: S => A, _get2: S => A)(_replace2: (B, B) => S => T): POptic[GetOneOrMore & Modify, S, T, A, B] = 
      POptic(
        new NonEmptyTraversalImpl:
          override def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] =
            Apply[F].map2(f(_get1(s)), f(_get2(s)))((b1, b2) => _replace2(b1, b2)(s)))

    def editOneOrMore[S, T, A, B, G[_]: NonEmptyTraverse](_getOneOrMore: S => G[A])(_replaceOneOrMore: G[B] => S => T): POptic[GetOneOrMore & Modify, S, T, A, B] = 
      edit(_getOneOrMore)(_replaceOneOrMore).andThen(nonEmptyTraverse)

    def editMany[S, T, A, B, G[_]: Traverse](_getMany: S => G[A])(_replaceMany: G[B] => S => T): POptic[GetMany & Modify, S, T, A, B] = 
      edit(_getMany)(_replaceMany).andThen(traverse)

    def convertBetween[S, T, A, B](_get: S => A)(_reverseGet: B => T): POptic[Get & ReverseGet, S, T, A, B] = 
      POptic(
        new IsoImpl: 
          self => 
          override def get(s: S): A = _get(s)
          override def reverseGet(b: B): T = _reverseGet(b)
          override def reverse: IsoImpl[B, A, T, S] = new IsoImpl:
            override def get(b: B): T = _reverseGet(b)
            override def reverseGet(a: S): A = _get(a)
            override def reverse: IsoImpl[S, T, A, B] = self)

    def selectBranch[S, T, A, B](_getOrModify: S => Either[T, A])(_reverseGet: B => T): POptic[GetOption & ReverseGet, S, T, A, B] =
      POptic(
        new PrismImpl:
          override def reverseGet(b: B): T = _reverseGet(b)
          override def getOrModify(s: S): Either[T, A] = _getOrModify(s)
          override def getOption(s: S): Option[A] = _getOrModify(s).toOption)

    def traverse[Tr[_]: Traverse, A, B]: POptic[GetMany & Modify, Tr[A], Tr[B], A, B] =
      POptic(
        new TraversalImpl:
          override def modifyA[F[+_]: Applicative](f: A => F[B])(s: Tr[A]): F[Tr[B]] =
            Traverse[Tr].traverse(s)(f))

    def nonEmptyTraverse[Tr[_]: NonEmptyTraverse, A, B]: POptic[GetOneOrMore & Modify, Tr[A], Tr[B], A, B] =
      POptic(
        new NonEmptyTraversalImpl:
          override def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: Tr[A]): F[Tr[B]] =
            NonEmptyTraverse[Tr].nonEmptyTraverse(s)(f))

// Fold:
  // def select[A](p: A => Boolean): Fold[A, A] =
  //   new Fold[A, A] {
  //     def foldMap[M: Monoid](f: A => M)(s: A): M =
  //       if (p(s)) f(s) else Monoid[M].empty
  //   }
// def fromFoldable[F[_]: Foldable, A]: Fold[F[A], A] =
//     new Fold[F[A], A] {
//       def foldMap[M: Monoid](f: A => M)(s: F[A]): M =
//         Foldable[F].foldMap(s)(f)
//     }

// PSetter:
// def fromFunctor[F[_], A, B](implicit F: Functor[F]): PSetter[F[A], F[B], A, B] =
//     PSetter[F[A], F[B], A, B](f => F.map(_)(f))

//   /** create a [[PSetter]] from a Contravariant functor */
//   def fromContravariant[F[_], A, B](implicit F: Contravariant[F]): PSetter[F[B], F[A], A, B] =
//     PSetter[F[B], F[A], A, B](f => F.contramap(_)(f))

//   /** create a [[PSetter]] from a Profunctor */
//   def fromProfunctor[P[_, _], A, B, C](implicit P: Profunctor[P]): PSetter[P[B, C], P[A, C], A, B] =
//     PSetter[P[B, C], P[A, C], A, B](f => P.lmap(_)(f))

// PTraversal:
  //   def apply3[S, T, A, B](get1: S => A, get2: S => A, get3: S => A)(_set: (B, B, B, S) => T): PTraversal[S, T, A, B] =
  //   new PTraversal[S, T, A, B] {
  //     def modifyA[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
  //       Applicative[F].map3(f(get1(s)), f(get2(s)), f(get3(s)))(_set(_, _, _, s))
  //   }

  // def apply4[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A)(
  //   _set: (B, B, B, B, S) => T
  // ): PTraversal[S, T, A, B] =
  //   new PTraversal[S, T, A, B] {
  //     def modifyA[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
  //       Applicative[F].map4(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)))(_set(_, _, _, _, s))
  //   }

  // def apply5[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A)(
  //   _set: (B, B, B, B, B, S) => T
  // ): PTraversal[S, T, A, B] =
  //   new PTraversal[S, T, A, B] {
  //     def modifyA[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
  //       Applicative[F].map5(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)))(_set(_, _, _, _, _, s))
  //   }

  // def apply6[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A, get6: S => A)(
  //   _set: (B, B, B, B, B, B, S) => T
  // ): PTraversal[S, T, A, B] =
  //   new PTraversal[S, T, A, B] {
  //     def modifyA[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
  //       Applicative[F].map6(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)), f(get6(s)))(
  //         _set(_, _, _, _, _, _, s)
  //       )
  //   }

// Traversal:
  /** Merge multiple Optionals together. All Optional must target different piece of data otherwise the Traversal
    * doesn't respect all properties. See this thread for more details:
    * https://github.com/julien-truffaut/Monocle/issues/379#issuecomment-236374838.
    */
  // def applyN[S, A](xs: Optional[S, A]*): Traversal[S, A] =
  //   new PTraversal[S, S, A, A] {
  //     def modifyA[F[_]: Applicative](f: A => F[A])(s: S): F[S] =
  //       xs.foldLeft(Applicative[F].pure(s))((fs, lens) =>
  //         Applicative[F].map2(lens.getOption(s).traverse(f), fs) {
  //           case (None, s)    => s
  //           case (Some(a), s) => lens.replace(a)(s)
  //         }
  //       )
  //   }

// Optional:
    // def void[S, A]: Optional[S, A] =
    //   Optional[S, A](_ => None)(_ => identity)
// def filter[A](predicate: A => Boolean): Optional[A, A] =
//     Optional[A, A](value => if (predicate(value)) Some(value) else None)(newValue =>
//       current => if (predicate(current)) newValue else current
//     )

//PLens:
  // def codiagonal[S, T]: PLens[Either[S, S], Either[T, T], S, T] =
  //   PLens[Either[S, S], Either[T, T], S, T](
  //     _.fold(identity, identity)
  //   )(t => _.bimap(_ => t, _ => t))

// Lens:
    // def codiagonal[S]: Lens[Either[S, S], S] =
    //   PLens.codiagonal

// Prism:
  //   /** Create a Prism using a partial function rather than Option. */
  // def partial[S, A](get: PartialFunction[S, A])(reverseGet: A => S): Prism[S, A] =
  //   Prism[S, A](get.lift)(reverseGet)

  // /** a [[Prism]] that checks for equality with a given value */
  // def only[A](a: A)(implicit A: Eq[A]): Prism[A, Unit] =
  //   Prism[A, Unit](a2 => if (A.eqv(a, a2)) Some(()) else None)(_ => a)

// Iso: 
    // def involuted[A](update: A => A): Iso[A, A] =
    // Iso(update)(update)

object Optic:
  def id[A]: Optic[Get & ReverseGet, A, A] = 
    POptic.id

  object thatCan:
    def get[S, A](_get: S => A): Optic[Get, S, A] = 
      POptic(
        new GetterImpl:
          override def get(s: S): A = _get(s))
          
    def get2[S, A](_get1: S => A, _get2: S => A): Optic[GetOneOrMore, S, A] = 
      POptic(
        new NonEmptyFoldImpl:
          override def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = 
            Semigroup[M].combine(f(_get1(s)), f(_get2(s))))

    def getOption[S, A](_getOption: S => Option[A]): Optic[GetOption, S, A] = 
      POptic(
        new OptionalGetterImpl:
          override def getOption(s: S): Option[A] = _getOption(s))

    def getOneOrMore[S, A](_getOneOrMore: S => NonEmptyList[A]): Optic[GetOneOrMore, S, A] = 
      POptic(
        new NonEmptyFoldImpl:
          override def nonEmptyFoldMap[M](f: A => M)(s: S)(using sem: Semigroup[M]): M = 
            val NonEmptyList(head, tail) = _getOneOrMore(s)
            tail.foldLeft(f(head))((m, a) => sem.combine(m, f(a))))

    def getMany[S, A](_getAll: S => List[A]): Optic[GetMany, S, A] = 
      POptic(
        new FoldImpl:
          override def foldMap[M](f: A => M)(s: S)(using mon: Monoid[M]): M = 
            _getAll(s).foldLeft(mon.empty)((m, a) => mon.combine(m, f(a))))

    def modify[S, A](_modify: (A => A) => S => S): Optic[Modify, S, A] = 
      POptic.thatCan.modify(_modify)

    def edit[S, A](_get: S => A)(_replace: A => S => S): Optic[Get & Modify, S, A] = 
      POptic.thatCan.edit(_get)(_replace)

    def editOption[S, A](_getOption: S => Option[A])(_replace: A => S => S): Optic[GetOption & Modify, S, A] = 
      POptic.thatCan.editOption[S, S, A, A](s => _getOption(s).fold(Left(s))(Right.apply))(_replace)

    def edit2[S, A](_get1: S => A, _get2: S => A)(_replace2: (A, A) => S => S): Optic[GetOneOrMore & Modify, S, A] = 
      POptic.thatCan.edit2(_get1, _get2)(_replace2)

    def editOneOrMore[S, A, G[_]: NonEmptyTraverse](_getOneOrMore: S => G[A])(_replaceOneOrMore: G[A] => S => S): Optic[GetOneOrMore & Modify, S, A] = 
      POptic.thatCan.editOneOrMore(_getOneOrMore)(_replaceOneOrMore)

    def editMany[S, A, G[_]: Traverse](_getMany: S => G[A])(_replaceMany: G[A] => S => S): Optic[GetMany & Modify, S, A] = 
      POptic.thatCan.editMany(_getMany)(_replaceMany)

    def convertBetween[S, A](_get: S => A)(_reverseGet: A => S): Optic[Get & ReverseGet, S, A] = 
      POptic.thatCan.convertBetween(_get)(_reverseGet)

    def selectBranch[S, A](_getOption: PartialFunction[S, A])(_reverseGet: A => S): Optic[GetOption & ReverseGet, S, A] =
      POptic(
        new PrismImpl:
          override def reverseGet(a: A): S = _reverseGet(a)
          override def getOrModify(s: S): Either[S, A] = _getOption.lift(s).fold(Left(s))(Right.apply)
          override def getOption(s: S): Option[A] = _getOption.lift(s))

    def traverse[Tr[_]: Traverse, A]: Optic[GetMany & Modify, Tr[A], A] =
      POptic.thatCan.traverse

    def nonEmptyTraverse[Tr[_]: NonEmptyTraverse, A]: Optic[GetOneOrMore & Modify, Tr[A], A] =
      POptic.thatCan.nonEmptyTraverse

