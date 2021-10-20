package monocle

import monocle.impl._
import monocle.functions.*
import monocle.internal.*

trait POpticConstructors:
  def modify[S, T, A, B](_modify: (A => B) => S => T): POptic[Modify, S, T, A, B] =
    POptic(new SetterImpl:
      override def modify(f: A => B)     = _modify(f)
      override def replace(b: B): S => T = _modify(_ => b)
    )

  def edit[S, T, A, B](_get: S => A)(_replace: B => S => T): POptic[Edit, S, T, A, B] =
    POptic(new LensImpl:
      override def get(s: S): A          = _get(s)
      override def replace(b: B): S => T = _replace(b)
      override def modifyF[F[+_]](f: A => F[B])(s: S)(using fun: Functor[F]): F[T] =
        fun.map(f(_get(s)))(a => _replace(a)(s))
      override def modify(f: A => B): S => T = s => _replace(f(_get(s)))(s)
    )

  def editOption[S, T, A, B](_getOrModify: S => Either[T, A])(
    _replace: B => S => T
  ): POptic[EditOption, S, T, A, B] =
    POptic(new OptionalImpl:
      override def getOrModify(s: S): Either[T, A] = _getOrModify(s)
      override def replace(b: B): S => T           = _replace(b)
      override def getOption(s: S): Option[A]      = _getOrModify(s).toOption
    )

  def edit2[S, T, A, B](_get1: S => A, _get2: S => A)(
    _replace2: (B, B) => S => T
  ): POptic[EditOneOrMore, S, T, A, B] =
    POptic(
      new NonEmptyTraversalImpl:
        override def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] =
          Apply[F].map2(f(_get1(s)), f(_get2(s)))((b1, b2) => _replace2(b1, b2)(s))
    )

  def editOneOrMore[S, T, A, B, G[_]: NonEmptyTraverse](_getOneOrMore: S => G[A])(
    _replaceOneOrMore: G[B] => S => T
  ): POptic[EditOneOrMore, S, T, A, B] =
    edit(_getOneOrMore)(_replaceOneOrMore).andThen(nonEmptyTraverse)

  def editMany[S, T, A, B, G[_]: Traverse](_getMany: S => G[A])(
    _replaceMany: G[B] => S => T
  ): POptic[EditMany, S, T, A, B] =
    edit(_getMany)(_replaceMany).andThen(traverse)

  def convertBetween[S, T, A, B](_get: S => A)(_reverseGet: B => T): POptic[ConvertBetween, S, T, A, B] =
    POptic(new IsoImpl:
      self =>
      override def get(s: S): A        = _get(s)
      override def reverseGet(b: B): T = _reverseGet(b)
      override def reverse: IsoImpl[B, A, T, S] = new IsoImpl:
        override def get(b: B): T                 = _reverseGet(b)
        override def reverseGet(a: S): A          = _get(a)
        override def reverse: IsoImpl[S, T, A, B] = self
    )

  def selectBranch[S, T, A, B](_getOrModify: S => Either[T, A])(
    _reverseGet: B => T
  ): POptic[SelectBranch, S, T, A, B] =
    POptic(new PrismImpl:
      override def reverseGet(b: B): T             = _reverseGet(b)
      override def getOrModify(s: S): Either[T, A] = _getOrModify(s)
      override def getOption(s: S): Option[A]      = _getOrModify(s).toOption
    )

  def traverse[Tr[_]: Traverse, A, B]: POptic[EditMany, Tr[A], Tr[B], A, B] =
    POptic(
      new TraversalImpl:
        override def modifyA[F[+_]: Applicative](f: A => F[B])(s: Tr[A]): F[Tr[B]] =
          Traverse[Tr].traverse(s)(f)
    )

  def nonEmptyTraverse[Tr[_]: NonEmptyTraverse, A, B]: POptic[EditOneOrMore, Tr[A], Tr[B], A, B] =
    POptic(
      new NonEmptyTraversalImpl:
        override def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: Tr[A]): F[Tr[B]] =
          NonEmptyTraverse[Tr].nonEmptyTraverse(s)(f)
    )
