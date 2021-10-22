package monocle

import monocle.impl._
import monocle.functions.*
import monocle.internal.*

trait FullOpticConstructors:
  def modify[Structure, Modified, Out, In](_modify: (Out => In) => Structure => Modified): FullOptic[Modify, Structure, Modified, Out, In] =
    FullOptic(new SetterImpl:
      override def modify(f: Out => In)     = _modify(f)
      override def replace(b: In): Structure => Modified = _modify(_ => b)
    )

  def edit[Structure, Modified, Out, In](_get: Structure => Out)(_replace: In => Structure => Modified): FullOptic[Get & Modify, Structure, Modified, Out, In] =
    FullOptic(new LensImpl:
      override def get(s: Structure): Out          = _get(s)
      override def replace(b: In): Structure => Modified = _replace(b)
      override def modifyF[F[+_]](f: Out => F[In])(s: Structure)(using fun: Functor[F]): F[Modified] =
        fun.map(f(_get(s)))(a => _replace(a)(s))
      override def modify(f: Out => In): Structure => Modified = s => _replace(f(_get(s)))(s)
    )

  def editOption[Structure, Modified, Out, In](_getOrModify: Structure => Either[Modified, Out])(
    _replace: In => Structure => Modified
  ): FullOptic[GetOption & Modify, Structure, Modified, Out, In] =
    FullOptic(new OptionalImpl:
      override def getOrModify(s: Structure): Either[Modified, Out] = _getOrModify(s)
      override def replace(b: In): Structure => Modified           = _replace(b)
      override def getOption(s: Structure): Option[Out]      = _getOrModify(s).toOption
    )

  def edit2[Structure, Modified, Out, In](_get1: Structure => Out, _get2: Structure => Out)(
    _replace2: (In, In) => Structure => Modified
  ): FullOptic[GetOneOrMore & Modify, Structure, Modified, Out, In] =
    FullOptic(
      new NonEmptyTraversalImpl:
        override def nonEmptyModifyA[F[+_]: Apply](f: Out => F[In])(s: Structure): F[Modified] =
          Apply[F].map2(f(_get1(s)), f(_get2(s)))((b1, b2) => _replace2(b1, b2)(s))
    )

  def editOneOrMore[Structure, Modified, Out, In, G[_]: NonEmptyTraverse](_getOneOrMore: Structure => G[Out])(
    _replaceOneOrMore: G[In] => Structure => Modified
  ): FullOptic[GetOneOrMore & Modify, Structure, Modified, Out, In] =
    edit(_getOneOrMore)(_replaceOneOrMore).andThen(nonEmptyTraverse)

  def editMany[Structure, Modified, Out, In, G[_]: Traverse](_getMany: Structure => G[Out])(
    _replaceMany: G[In] => Structure => Modified
  ): FullOptic[GetMany & Modify, Structure, Modified, Out, In] =
    edit(_getMany)(_replaceMany).andThen(traverse)

  def convertBetween[Structure, Modified, Out, In](_get: Structure => Out)(_reverseGet: In => Modified): FullOptic[Get & ReverseGet, Structure, Modified, Out, In] =
    FullOptic(new IsoImpl:
      self =>
      override def get(s: Structure): Out        = _get(s)
      override def reverseGet(b: In): Modified = _reverseGet(b)
      override def reverse: IsoImpl[In, Out, Modified, Structure] = new IsoImpl:
        override def get(b: In): Modified                 = _reverseGet(b)
        override def reverseGet(a: Structure): Out          = _get(a)
        override def reverse: IsoImpl[Structure, Modified, Out, In] = self
    )

  def selectBranch[Structure, Modified, Out, In](_getOrModify: Structure => Either[Modified, Out])(
    _reverseGet: In => Modified
  ): FullOptic[GetOption & ReverseGet, Structure, Modified, Out, In] =
    FullOptic(new PrismImpl:
      override def reverseGet(b: In): Modified             = _reverseGet(b)
      override def getOrModify(s: Structure): Either[Modified, Out] = _getOrModify(s)
      override def getOption(s: Structure): Option[Out]      = _getOrModify(s).toOption
    )

  def traverse[T[_]: Traverse, Out, In]: FullOptic[GetMany & Modify, T[Out], T[In], Out, In] =
    FullOptic(
      new TraversalImpl:
        override def modifyA[F[+_]: Applicative](f: Out => F[In])(s: T[Out]): F[T[In]] =
          Traverse[T].traverse(s)(f)
    )

  def nonEmptyTraverse[T[_]: NonEmptyTraverse, Out, In]: FullOptic[GetOneOrMore & Modify, T[Out], T[In], Out, In] =
    FullOptic(
      new NonEmptyTraversalImpl:
        override def nonEmptyModifyA[F[+_]: Apply](f: Out => F[In])(s: T[Out]): F[T[In]] =
          NonEmptyTraverse[T].nonEmptyTraverse(s)(f)
    )

end FullOpticConstructors