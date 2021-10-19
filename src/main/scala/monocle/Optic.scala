package monocle

import monocle.impl._
import monocle.functions.*
import monocle.internal.*

type Optic[+ThisCan, S, A]      = POptic[ThisCan, S, S, A, A]
type OpticGet[+ThisCan, -S, +A] = POptic[ThisCan, S, Nothing, A, Any]

object GetOptic:
  def id[A]: OpticGet[Get, A, A] =
    Optic.thatCan.get[A, A](identity)

object Optic:
  def id[A]: Optic[Get & ReverseGet, A, A] =
    POptic.id

  object thatCan extends OpticConstructors(POptic.thatCan)

end Optic

object POptic:
  def id[A, B]: POptic[Get & ReverseGet, A, B, A, B] =
    thatCan.convertBetween[A, B, A, B](identity)(identity)

  object thatCan extends POpticConstructors

end POptic

final class POptic[+ThisCan, -S, +T, +A, -B] private[monocle] (
  protected[monocle] val impl: OpticImpl[ThisCan, S, T, A, B]
):

  def andThen[ThatCan >: ThisCan, C, D](that: POptic[ThatCan, A, B, C, D]): POptic[ThatCan, S, T, C, D] =
    POptic(impl.andThen(that.impl))

  def index[UnifiedS >: T <: S, UnifiedA >: A <: B, I, V](i: I)(using evIndex: Index[UnifiedA, I, V]): Optic[ThisCan | (GetOption & Modify), UnifiedS, V] =
    andThen(evIndex.index(i))

  override def toString() =
    s"POptic(${impl})"

end POptic

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def all(p: A => Boolean): S => Boolean =
    self.impl.safe.toIterator(_).forall(p)

extension [S, A](self: Optic[Get, S, A])
  def choice[S1](other: Optic[Get, S1, A]): Optic[Get, Either[S, S1], A] =
    Optic.thatCan.get(_.fold(self.get, other.get))

extension [ThisCan, S, A](self: Optic[ThisCan, S, A])
  def each[C](using evEach: Each[A, C]): Optic[ThisCan | (GetMany & Modify), S, C] =
    self.andThen(evEach.each)

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def exist(p: A => Boolean): S => Boolean =
    self.impl.safe.toIterator(_).exists(p)

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def find(p: A => Boolean): S => Option[A] =
    self.impl.safe.toIterator(_).find(p)

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def fold(s: S)(using Monoid[A]): A =
    foldMap(identity)(s)

  def foldMap[M: Monoid](f: A => M)(s: S): M = {
    var result: M = Monoid[M].empty
    val it        = self.impl.safe.toIterator(s)
    while (it.hasNext) result = Monoid[M].combine(result, f(it.next()))
    result
  }

extension [S, T, A, B](self: POptic[Get, S, T, A, B])
  def get(s: S): A =
    self.impl.safe.get(s)

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def getAll(s: S): List[A] =
    self.impl.safe.toIterator(s).toList

extension [S, T, A, B](self: POptic[GetOneOrMore, S, T, A, B])
  def getOneOrMore(s: S): NonEmptyList[A] =
    self.impl.safe.nonEmptyFoldMap[NonEmptyList[A]](NonEmptyList(_, Nil))(s)

extension [S, T, A, B](self: POptic[GetOption, S, T, A, B])
  def getOption(s: S): Option[A] =
    self.impl.safe.getOption(s)

extension [S, T, A, B](self: POptic[GetOption & Modify, S, T, A, B])
  def getOrModify(s: S): Either[T, A] =
    self.impl.safe.getOrModify(s)

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def headOption(s: S): Option[A] = {
    val it = self.impl.safe.toIterator(s)
    if (it.hasNext) Some(it.next()) else None
  }

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def isEmpty(s: S): Boolean =
    self.impl.safe.toIterator(s).isEmpty

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def lastOption(s: S): Option[A] = {
    var result: Option[A] = None
    for (value <- self.impl.safe.toIterator(s)) result = Some(value)
    result
  }

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def length(s: S): Int =
    self.impl.safe.toIterator(s).length

extension [S, T, A, B](self: POptic[GetMany, S, T, A, B])
  def nonEmpty(s: S): Boolean =
    !self.isEmpty(s)

extension [S, T, A, B](self: POptic[GetOneOrMore, S, T, A, B])
  def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M =
    self.impl.safe.nonEmptyFoldMap(f)(s)

extension [S, T, A, B](self: POptic[GetOneOrMore & Modify, S, T, A, B])
  def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] =
    self.impl.safe.nonEmptyModifyA(f)(s)

extension [S, T, A, B](self: POptic[Get & ReverseGet, S, T, A, B])
  def mapping[F[_]: Functor]: POptic[Get & ReverseGet, F[S], F[T], F[A], F[B]] =
    POptic.thatCan.convertBetween[F[S], F[T], F[A], F[B]](fs => Functor[F].map(fs)(self.get))(fb =>
      Functor[F].map(fb)(self.reverseGet)
    )

extension [S, T, A, B](self: POptic[Modify, S, T, A, B])
  def modify(f: A => B): S => T =
    self.impl.safe.modify(f)

extension [S, T, A, B](self: POptic[GetMany & Modify, S, T, A, B])
  def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    self.impl.safe.modifyA(f)(s)

extension [S, T, A, B](self: POptic[Get & Modify, S, T, A, B])
  def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T] =
    self.impl.safe.modifyF(f)(s)

extension [S, T, A, B](self: POptic[GetOption & Modify, S, T, A, B])
  def modifyOption(f: A => B): S => Option[T] =
    s => self.impl.safe.getOption(s).map(a => self.impl.safe.replace(f(a))(s))

extension [S, T, A, B](self: POptic[GetOption & Modify, S, T, A, B])
  def orElse(other: POptic[GetOption & Modify, S, T, A, B]): POptic[GetOption & Modify, S, T, A, B] =
    POptic.thatCan.editOption[S, T, A, B](s => self.getOrModify(s).orElse(other.getOrModify(s)))(b =>
      s => self.replaceOption(b)(s).getOrElse(other.replace(b)(s))
    )

extension [S, T, A, B](self: POptic[GetMany & Modify, S, T, A, B])
  def parModifyF[G[+_]](f: A => G[B])(s: S)(using par: Parallel[G]): G[T] =
    par.sequential(self.modifyA(a => par.parallel(f(a)))(s)(par.applicative))

extension [S, T, A, B](self: POptic[GetOneOrMore & Modify, S, T, A, B])
  def parNonEmptyModifyF[F[+_]](f: A => F[B])(s: S)(using par: NonEmptyParallel[F]): F[T] =
    par.sequential(self.nonEmptyModifyA(a => par.parallel(f(a)))(s)(par.apply))

extension [S, T, A, B](self: POptic[ReverseGet, S, T, A, B])
  def re: Optic[Get, B, T] =
    Optic.thatCan.get(self.reverseGet)

extension [S, T, A, B](self: POptic[Modify, S, T, A, B])
  def replace(b: B): S => T =
    self.impl.safe.modify(_ => b)

extension [S, T, A, B](self: POptic[GetOption & Modify, S, T, A, B])
  def replaceOption(b: B): S => Option[T] =
    self.modifyOption(_ => b)

extension [S, T, A, B](self: POptic[ReverseGet, S, T, A, B])
  def reverseGet(b: B): T =
    self.impl.safe.reverseGet(b)

extension [S, T, A, B](self: POptic[Get, S, T, A, B])
  def split[S1, A1](other: Optic[Get, S1, A1]): Optic[Get, (S, S1), (A, A1)] =
    Optic.thatCan.get((s, s1) => (self.get(s), other.get(s1)))

extension [ThisCan, S, T, A, B](self: POptic[ThisCan, S, T, Option[A], Option[B]])
  def some: POptic[ThisCan | GetOption, S, T, A, B] =
    self.andThen(std.option.pSome)

extension [ThisCan <: GetMany, NewCan >: ThisCan | Get, S, T, A, B](self: POptic[ThisCan, S, T, A, B])
  def to[C](f: A => C): POptic[NewCan, S, Any, C, Nothing] =
    self.andThen(Optic.thatCan.get(f))

extension [ThisCan, S, T, A, B](self: POptic[ThisCan, S, T, Option[A], Option[B]])
  def withDefault(defaultValue: A): POptic[ThisCan | (Get & ReverseGet), S, T, A, B] =
    val iso = POptic.thatCan.convertBetween[Option[A], Option[B], A, B](_.getOrElse(defaultValue))(Some.apply)
    self.andThen(iso)

extension [S, A](self: Optic[Get, S, A])
  def zip[A1](other: Optic[Get, S, A1]): Optic[Get, S, (A, A1)] =
    Optic.thatCan.get(s => (self.get(s), other.get(s)))

// Optional overrides: isEmpty, nonEmpty, find, exist, all
// Lens overrides: modifyA, foldMap, find, exist
// Prism overrides: modifyA, modify, replace
// Iso overrides: foldMap, find, exist, modifyF, modifyA, modify, replace

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
/** Merge multiple Optionals together. All Optional must target different piece of data otherwise the Traversal doesn't
  * respect all properties. See this thread for more details:
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
