package monocle

import monocle.impl._
import monocle.functions.*
import monocle.internal.*

trait OpticConstructors(fullConstructors: FullOpticConstructors):

  def get[Structure, Out](_get: Structure => Out): OpticGet[Get, Structure, Out] =
    FullOptic(
      new GetterImpl:
        override def get(s: Structure): Out = _get(s)
    )

  def get2[Structure, Out](_get1: Structure => Out, _get2: Structure => Out): OpticGet[GetOneOrMore, Structure, Out] =
    getOneOrMore(s => NonEmptyList.of(_get1(s), _get2(s)))

  def getOption[Structure, Out](_getOption: Structure => Option[Out]): OpticGet[GetOption, Structure, Out] =
    FullOptic(
      new OptionalGetterImpl:
        override def getOption(s: Structure): Option[Out] = _getOption(s)
    )

  def getOneOrMore[Structure, Out](_getOneOrMore: Structure => NonEmptyList[Out]): OpticGet[GetOneOrMore, Structure, Out] =
    FullOptic(
      new NonEmptyFoldImpl:
        override def nonEmptyFoldMap[M](f: Out => M)(s: Structure)(using sem: Semigroup[M]): M =
          val NonEmptyList(head, tail) = _getOneOrMore(s)
          tail.foldLeft(f(head))((m, out) => sem.combine(m, f(out)))

        override def toIterator(s: Structure): Iterator[Out] =
          _getOneOrMore(s).iterator
    )

  def getMany[Structure, Out](_getAll: Structure => IterableOnce[Out]): OpticGet[GetMany, Structure, Out] =
    FullOptic(
      new FoldImpl:
        override def toIterator(s: Structure): Iterator[Out] =
          _getAll(s).iterator
    )

  def modify[Structure, A](_modify: (A => A) => Structure => Structure): Optic[Modify, Structure, A] =
    fullConstructors.modify(_modify)

  def edit[Structure, A](_get: Structure => A)(_replace: A => Structure => Structure): Optic[Get & Modify, Structure, A] =
    fullConstructors.edit(_get)(_replace)

  def editOption[Structure, A](_getOption: Structure => Option[A])(_replace: A => Structure => Structure): Optic[GetOption & Modify, Structure, A] =
    fullConstructors.editOption[Structure, Structure, A, A](s => _getOption(s).fold(Left(s))(Right.apply))(_replace)

  def edit2[Structure, A](_get1: Structure => A, _get2: Structure => A)(_replace2: (A, A) => Structure => Structure): Optic[GetOneOrMore & Modify, Structure, A] =
    fullConstructors.edit2(_get1, _get2)(_replace2)

  def editOneOrMore[Structure, A, G[_]: NonEmptyTraverse](_getOneOrMore: Structure => G[A])(
    _replaceOneOrMore: G[A] => Structure => Structure
  ): Optic[GetOneOrMore & Modify, Structure, A] =
    fullConstructors.editOneOrMore(_getOneOrMore)(_replaceOneOrMore)

  def editMany[Structure, A, G[_]: Traverse](_getMany: Structure => G[A])(_replaceMany: G[A] => Structure => Structure): Optic[GetMany & Modify, Structure, A] =
    fullConstructors.editMany(_getMany)(_replaceMany)

  def convertBetween[Structure, A](_get: Structure => A)(_reverseGet: A => Structure): Optic[Get & ReverseGet, Structure, A] =
    fullConstructors.convertBetween(_get)(_reverseGet)

  def selectBranch[Structure, A](_getOption: PartialFunction[Structure, A])(_reverseGet: A => Structure): Optic[GetOption & ReverseGet, Structure, A] =
    FullOptic(new PrismImpl:
      override def reverseGet(out: A): Structure             = _reverseGet(out)
      override def getOrModify(s: Structure): Either[Structure, A] = _getOption.lift(s).fold(Left(s))(Right.apply)
      override def getOption(s: Structure): Option[A]      = _getOption.lift(s)
    )

  def traverse[T[_]: Traverse, A]: Optic[GetMany & Modify, T[A], A] =
    fullConstructors.traverse

  def nonEmptyTraverse[T[_]: NonEmptyTraverse, A]: Optic[GetOneOrMore & Modify, T[A], A] =
    fullConstructors.nonEmptyTraverse
