package monocle

import monocle.impl._
import monocle.functions.*
import monocle.internal.*

type Optic[+Can, Structure, A] = FullOptic[Can, Structure, Structure, A, A]
type OpticGet[+Can, -Structure, +Out] = FullOptic[Can, Structure, Nothing, Out, Any]

object OpticGet:
  def id[A]: OpticGet[Get, A, A] =
    Optic.thatCan.get[A, A](identity)

object Optic:
  def id[A]: Optic[Get & ReverseGet, A, A] =
    FullOptic.id

  object thatCan extends OpticConstructors(FullOptic.thatCan)

end Optic

object FullOptic:
  def id[A, B]: FullOptic[Get & ReverseGet, A, B, A, B] =
    thatCan.convertBetween[A, B, A, B](identity)(identity)

  object thatCan extends FullOpticConstructors

end FullOptic

final class FullOptic[+Can, -Structure, +Modified, +Out, -In] private[monocle] (
    private val impl: OpticImpl[Can, Structure, Modified, Out, In]):

  def andThen[Can2 >: Can, Out2, In2](optic2: FullOptic[Can2, Out, In, Out2, In2]): FullOptic[Can2, Structure, Modified, Out2, In2] =
    FullOptic(impl.andThen(optic2.impl))

  def all(p: Out => Boolean)(using Can <:< GetMany): Structure => Boolean =
    impl.safe.toIterator(_).forall(p)

  def choice[Structure1 >: Modified <: Structure, A >: Out <: In, Structure2]
        (optic2: Optic[Get, Structure2, A])
        (using Can <:< Get): Optic[Get, Either[Structure1, Structure2], A] =
    Optic.thatCan.get(_.fold(this.get, optic2.get))

  def each[Structure1 >: Modified <: Structure, A >: Out <: In, A2]
      (using evEach: Each[A, A2]): Optic[Can | (GetMany & Modify), Structure1, A2] =
    andThen(evEach.each)

  def exist(p: Out => Boolean)(using Can <:< GetMany): Structure => Boolean =
    impl.safe.toIterator(_).exists(p)

  def find(p: Out => Boolean)(using Can <:< GetMany): Structure => Option[Out] =
    impl.safe.toIterator(_).find(p)

  def fold[Out1 >: Out : Monoid](s: Structure)(using Can <:< GetMany): Out1 = 
    this.foldMap[Out1](identity)(s)

  def foldMap[M: Monoid](f: Out => M)(s: Structure)(using Can <:< GetMany): M = {
    var result: M = Monoid[M].empty
    val it        = impl.safe.toIterator(s)
    while (it.hasNext) result = Monoid[M].combine(result, f(it.next()))
    result
  }

  def get(s: Structure)(using Can <:< Get): Out =
    impl.safe.get(s)

  def getAll(s: Structure)(using Can <:< GetMany): List[Out] =
    impl.safe.toIterator(s).toList

  def getOneOrMore(s: Structure)(using Can <:< GetOneOrMore): NonEmptyList[Out] =
    impl.safe.nonEmptyFoldMap[NonEmptyList[Out]](NonEmptyList(_, Nil))(s)

  def getOption(s: Structure)(using Can <:< GetOption): Option[Out] =
    impl.safe.getOption(s)

  def getOrModify(s: Structure)(using Can <:< (GetOption & Modify)): Either[Modified, Out] =
    impl.safe.getOrModify(s)

  def head(s: Structure)(using Can <:< GetOneOrMore): Out = {
    val it = impl.safe.toIterator(s)
    it.next()
  }

  def headOption(s: Structure)(using Can <:< GetMany): Option[Out] = {
    val it = impl.safe.toIterator(s)
    if (it.hasNext) Some(it.next()) else None
  }

  def index[Structure1 >: Modified <: Structure, A >: Out <: In, Idx, A2]
      (i: Idx)
      (using evIndex: Index[A, Idx, A2]): Optic[Can | (GetOption & Modify), Structure1, A2] =
    andThen(evIndex.index(i))

  def isEmpty(s: Structure)(using Can <:< GetMany): Boolean =
    impl.safe.toIterator(s).isEmpty

  def lastOption(s: Structure)(using Can <:< GetMany): Option[Out] = {
    var result: Option[Out] = None
    for (value <- impl.safe.toIterator(s)) result = Some(value)
    result
  }

  def length(s: Structure)(using Can <:< GetMany): Int =
    impl.safe.toIterator(s).length

  def mapping[F[+_]: Functor]
      (using Can <:< (Get & ReverseGet)): FullOptic[Get & ReverseGet, F[Structure], F[Modified], F[Out], F[In]] =
    FullOptic.thatCan.convertBetween[F[Structure], F[Modified], F[Out], F[In]]
      (fs => Functor[F].map(fs)(get))
      (fin => Functor[F].map(fin)(this.reverseGet))

  def modify(f: Out => In)(using Can <:< Modify): Structure => Modified =
    impl.safe.modify(f)

  def modifyA[F[+_]: Applicative]
      (f: Out => F[In])
      (s: Structure)
      (using Can <:< (GetMany & Modify)): F[Modified] =
    impl.safe.modifyA(f)(s)

  def modifyF[F[+_]: Functor]
      (f: Out => F[In])
      (s: Structure)
      (using Can <:< (Get & Modify)): F[Modified] =
    impl.safe.modifyF(f)(s)

  def modifyOption(f: Out => In)(using Can <:< (GetOption & Modify)): Structure => Option[Modified] =
    s => impl.safe.getOption(s).map(out => impl.safe.replace(f(out))(s))

  def nonEmpty(s: Structure)(using Can <:< GetMany): Boolean =
    !isEmpty(s)

  def nonEmptyFoldMap[M: Semigroup](f: Out => M)(s: Structure)(using Can <:< GetOneOrMore): M =
    impl.safe.nonEmptyFoldMap(f)(s)

  def nonEmptyModifyA[F[+_]: Apply]
      (f: Out => F[In])
      (s: Structure)
      (using Can <:< (GetOneOrMore & Modify)): F[Modified] =
    impl.safe.nonEmptyModifyA(f)(s)
  
  def orElse[Structure1 <: Structure, Modified1 >: Modified, Out1 >: Out, In1 <: In]
      (other: FullOptic[GetOption & Modify, Structure1, Modified1, Out1, In1])
      (using Can <:< (GetOption & Modify)): FullOptic[GetOption & Modify, Structure1, Modified1, Out1, In1] =
    FullOptic.thatCan.editOption[Structure1, Modified1, Out1, In1]
      (s => this.getOrModify(s).orElse(other.getOrModify(s)))
      (in => s => this.replaceOption(in)(s).getOrElse(other.replace(in)(s)))

  def parModifyF[G[+_]]
      (f: Out => G[In])
      (s: Structure)
      (using p: Parallel[G])
      (using Can <:< (GetMany & Modify)): G[Modified] =
    given Applicative[p.F] = p.applicative
    p.sequential(this.modifyA(out => p.parallel(f(out)))(s))

  def parNonEmptyModifyF[F[+_]]
      (f: Out => F[In])
      (s: Structure)
      (using p: NonEmptyParallel[F])
      (using Can <:< (GetOneOrMore & Modify)): F[Modified] =
    given Apply[p.F] = p.apply
    p.sequential(this.nonEmptyModifyA(out => p.parallel(f(out)))(s))

  def re[T1 >: Modified, B1 <: In](using Can <:< ReverseGet): Optic[Get, B1, T1] =
    Optic.thatCan.get(this.reverseGet)

  def replace(in: In)(using Can <:< Modify): Structure => Modified =
    impl.safe.modify(_ => in)

  def replaceOption(in: In)(using Can <:< (GetOption & Modify)): Structure => Option[Modified] =
    this.modifyOption(_ => in)

  def reverseGet(in: In)(using Can <:< ReverseGet): Modified =
    impl.safe.reverseGet(in)

  def some[A1, B1](using Out <:< Option[A1], Option[B1] <:< In): FullOptic[Can | GetOption, Structure, Modified, A1, B1] =
    asInstanceOf[FullOptic[Can, Structure, Modified, Option[A1], Option[B1]]].andThen(std.option.pSome)

  def split[Structure1 >: Modified <: Structure, A >: Out <: In, Structure2, A2]
      (other: Optic[Get, Structure2, A2])
      (using Can <:< Get): Optic[Get, (Structure1, Structure2), (A, A2)] =
    Optic.thatCan.get((s, s1) => (this.get(s), other.get(s1)))

  def to[C](f: Out => C)(using Can <:< GetMany): FullOptic[Can | Get, Structure, Any, C, Nothing] =
    this.andThen(Optic.thatCan.get(f))

  override def toString() =
    s"FullOptic(${impl})"

  def withDefault[Out1 >: Out, In1 <: In]
      (defaultValue: Out1)
      (using Out <:< Option[Out1]): FullOptic[Can | (Get & ReverseGet), Structure, Modified, Out1, In1] = 
    val iso = FullOptic.thatCan.convertBetween[Option[Out1], Option[In1], Out1, In1](_.getOrElse(defaultValue))(Some.apply)
    asInstanceOf[FullOptic[Can, Structure, Modified, Option[Out1], Option[In1]]].andThen(iso)

  def zip[Structure1 >: Modified <: Structure, A >: Out <: In, A2]
      (optic2: Optic[Get, Structure1, A2])
      (using Can <:< Get): Optic[Get, Structure1, (A, A2)] =
    Optic.thatCan.get(s => (this.get(s), optic2.get(s)))

end FullOptic















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
//   def apply3[Structure, Modified, A, B](get1: Structure => A, get2: Structure => A, get3: Structure => A)(_set: (B, B, B, Structure) => Modified): PTraversal[Structure, Modified, A, B] =
//   new PTraversal[Structure, Modified, A, B] {
//     def modifyA[F[_]: Applicative](f: A => F[B])(s: Structure): F[Modified] =
//       Applicative[F].map3(f(get1(s)), f(get2(s)), f(get3(s)))(_set(_, _, _, s))
//   }

// def apply4[Structure, Modified, A, B](get1: Structure => A, get2: Structure => A, get3: Structure => A, get4: Structure => A)(
//   _set: (B, B, B, B, Structure) => Modified
// ): PTraversal[Structure, Modified, A, B] =
//   new PTraversal[Structure, Modified, A, B] {
//     def modifyA[F[_]: Applicative](f: A => F[B])(s: Structure): F[Modified] =
//       Applicative[F].map4(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)))(_set(_, _, _, _, s))
//   }

// def apply5[Structure, Modified, A, B](get1: Structure => A, get2: Structure => A, get3: Structure => A, get4: Structure => A, get5: Structure => A)(
//   _set: (B, B, B, B, B, Structure) => Modified
// ): PTraversal[Structure, Modified, A, B] =
//   new PTraversal[Structure, Modified, A, B] {
//     def modifyA[F[_]: Applicative](f: A => F[B])(s: Structure): F[Modified] =
//       Applicative[F].map5(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)))(_set(_, _, _, _, _, s))
//   }

// def apply6[Structure, Modified, A, B](get1: Structure => A, get2: Structure => A, get3: Structure => A, get4: Structure => A, get5: Structure => A, get6: Structure => A)(
//   _set: (B, B, B, B, B, B, Structure) => Modified
// ): PTraversal[Structure, Modified, A, B] =
//   new PTraversal[Structure, Modified, A, B] {
//     def modifyA[F[_]: Applicative](f: A => F[B])(s: Structure): F[Modified] =
//       Applicative[F].map6(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)), f(get6(s)))(
//         _set(_, _, _, _, _, _, s)
//       )
//   }

// Traversal:
/** Merge multiple Optionals together. All Optional must target different piece of data otherwise the Traversal doesn't
  * respect all properties. See this thread for more details:
  * https://github.com/julien-truffaut/Monocle/issues/379#issuecomment-236374838.
  */
// def applyN[Structure, A](xs: Optional[Structure, A]*): Traversal[Structure, A] =
//   new PTraversal[Structure, Structure, A, A] {
//     def modifyA[F[_]: Applicative](f: A => F[A])(s: Structure): F[Structure] =
//       xs.foldLeft(Applicative[F].pure(s))((fs, lens) =>
//         Applicative[F].map2(lens.getOption(s).traverse(f), fs) {
//           case (None, s)    => s
//           case (Some(a), s) => lens.replace(a)(s)
//         }
//       )
//   }

// Optional:
// def void[Structure, A]: Optional[Structure, A] =
//   Optional[Structure, A](_ => None)(_ => identity)
// def filter[A](predicate: A => Boolean): Optional[A, A] =
//     Optional[A, A](value => if (predicate(value)) Some(value) else None)(newValue =>
//       current => if (predicate(current)) newValue else current
//     )

//PLens:
// def codiagonal[Structure, Modified]: PLens[Either[Structure, Structure], Either[Modified, Modified], Structure, Modified] =
//   PLens[Either[Structure, Structure], Either[Modified, Modified], Structure, Modified](
//     _.fold(identity, identity)
//   )(t => _.bimap(_ => t, _ => t))

// Lens:
// def codiagonal[Structure]: Lens[Either[Structure, Structure], Structure] =
//   PLens.codiagonal

// Prism:
//   /** Create a Prism using a partial function rather than Option. */
// def partial[Structure, A](get: PartialFunction[Structure, A])(reverseGet: A => Structure): Prism[Structure, A] =
//   Prism[Structure, A](get.lift)(reverseGet)

// /** a [[Prism]] that checks for equality with a given value */
// def only[A](a: A)(implicit A: Eq[A]): Prism[A, Unit] =
//   Prism[A, Unit](a2 => if (A.eqv(a, a2)) Some(()) else None)(_ => a)

// Iso:
// def involuted[A](update: A => A): Iso[A, A] =
// Iso(update)(update)
