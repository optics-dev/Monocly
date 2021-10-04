package monocly

import monocly.impl._
import monocly.functions.*
import monocly.internal.*

type Optic[+ThisCan, S, A] = POptic[ThisCan, S, S, A, A]

final class POptic[+ThisCan, -S, +T, +A, -B] private[monocly](protected[monocly] val impl: OpticImpl[S, T, A, B]):

  def andThen[ThatCan >: ThisCan, C, D](o: POptic[ThatCan, A, B, C, D]): POptic[ThatCan, S, T, C, D] =
    POptic(impl.andThen(o.impl))

  def each[C, S1 <: S, A1 >: A](using evEach: Each[A1, C])(using T <:< S1, A1 <:< B): Optic[ThisCan | (GetMany & Modify), S1, C] = 
    asInstanceOf[Optic[ThisCan, S1, A1]].andThen(evEach.each)

  def foldMap[M: Monoid](f: A => M)(s: S)(using ThisCan <:< GetMany): M = 
    impl.foldMap(f)(s)

  def get(using ThisCan <:< Get): S => A = 
    impl.get

  def getAll(using ThisCan <:< GetMany): S => List[A] = 
    impl.foldMap[List[A]](List(_))

  def getOneOrMore(using ThisCan <:< GetOneOrMore): S => NonEmptyList[A] = 
    impl.nonEmptyFoldMap[NonEmptyList[A]](NonEmptyList(_, Nil))

  def getOption(using ThisCan <:< GetOption): S => Option[A] = 
    impl.getOption

  def getOrModify(using ThisCan <:< GetOption & Modify): S => Either[T, A] = 
    impl.getOrModify

  def index[I, S1 <: S, A1 >: A, V](i: I)(using evIndex: Index[A1, I, V])(using T <:< S1, A1 <:< B): Optic[ThisCan | (GetOption & Modify), S1, V] = 
    asInstanceOf[Optic[ThisCan, S1, A1]].andThen(evIndex.index(i))

  def modify(f: A => B)(using ThisCan <:< Modify): S => T = 
    impl.modify(f)

  def modifyA[F[+_]: Applicative](f: A => F[B])(s: S)(using ThisCan <:< GetMany & Modify): F[T] = 
    impl.modifyA(f)(s)

  def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S)(using ThisCan <:< GetOneOrMore): M = 
    impl.nonEmptyFoldMap(f)(s)

  def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S)(using ThisCan <:< GetOneOrMore & Modify): F[T] = 
    impl.nonEmptyModifyA(f)(s)

  def replace(b: B)(using ThisCan <:< Modify): S => T = 
    impl.modify(_ => b)

  def reverseGet(using ThisCan <:< ReverseGet): B => T = 
    impl.reverseGet

  def some[A1, B1](using A <:< Option[A1], Option[B1] <:< B): POptic[ThisCan | GetOption, S, T, A1, B1] = 
    asInstanceOf[POptic[ThisCan, S, T, Option[A1], Option[B1]]].andThen(std.option.pSome)

  def withDefault[A1 >: A, B1 <: B](defaultValue: A1)(using A <:< Option[A1]): POptic[ThisCan | (Get & ReverseGet), S, T, A1, B1] = 
    val iso = POptic.thatCan.convertBetween[Option[A1], Option[B1], A1, B1](_.getOrElse(defaultValue))(Some.apply)
    asInstanceOf[POptic[ThisCan, S, T, Option[A1], Option[B1]]].andThen(iso)

end POptic

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
      POptic(
        new OptionalImpl:
          override def getOrModify(s: S): Either[S, A] = _getOption(s).fold(Left(s))(Right.apply)
          override def replace(a: A): S => S = _replace(a)
          override def getOption(s: S): Option[A] = _getOption(s))

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

