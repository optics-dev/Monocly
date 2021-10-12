package monocle

import monocle.impl._
import monocle.functions.*
import monocle.internal.*

trait OpticConstructors(polyConstructors: POpticConstructors):

  def get[S, A](_get: S => A): OpticGet[Get, S, A] = 
    POptic(
      new GetterImpl:
        override def get(s: S): A = _get(s))
        
  def get2[S, A](_get1: S => A, _get2: S => A): OpticGet[GetOneOrMore, S, A] = 
    POptic(
      new NonEmptyFoldImpl:
        override def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = 
          Semigroup[M].combine(f(_get1(s)), f(_get2(s))))

  def getOption[S, A](_getOption: S => Option[A]): OpticGet[GetOption, S, A] = 
    POptic(
      new OptionalGetterImpl:
        override def getOption(s: S): Option[A] = _getOption(s))

  def getOneOrMore[S, A](_getOneOrMore: S => NonEmptyList[A]): OpticGet[GetOneOrMore, S, A] = 
    POptic(
      new NonEmptyFoldImpl:
        override def nonEmptyFoldMap[M](f: A => M)(s: S)(using sem: Semigroup[M]): M = 
          val NonEmptyList(head, tail) = _getOneOrMore(s)
          tail.foldLeft(f(head))((m, a) => sem.combine(m, f(a))))

  def getMany[S, A](_getAll: S => List[A]): OpticGet[GetMany, S, A] = 
    POptic(
      new FoldImpl:
        override def foldMap[M](f: A => M)(s: S)(using mon: Monoid[M]): M = 
          _getAll(s).foldLeft(mon.empty)((m, a) => mon.combine(m, f(a))))

  def modify[S, A](_modify: (A => A) => S => S): Optic[Modify, S, A] = 
    polyConstructors.modify(_modify)

  def edit[S, A](_get: S => A)(_replace: A => S => S): Optic[Get & Modify, S, A] = 
    polyConstructors.edit(_get)(_replace)

  def editOption[S, A](_getOption: S => Option[A])(_replace: A => S => S): Optic[GetOption & Modify, S, A] = 
    polyConstructors.editOption[S, S, A, A](s => _getOption(s).fold(Left(s))(Right.apply))(_replace)

  def edit2[S, A](_get1: S => A, _get2: S => A)(_replace2: (A, A) => S => S): Optic[GetOneOrMore & Modify, S, A] = 
    polyConstructors.edit2(_get1, _get2)(_replace2)

  def editOneOrMore[S, A, G[_]: NonEmptyTraverse](_getOneOrMore: S => G[A])(_replaceOneOrMore: G[A] => S => S): Optic[GetOneOrMore & Modify, S, A] = 
    polyConstructors.editOneOrMore(_getOneOrMore)(_replaceOneOrMore)

  def editMany[S, A, G[_]: Traverse](_getMany: S => G[A])(_replaceMany: G[A] => S => S): Optic[GetMany & Modify, S, A] = 
    polyConstructors.editMany(_getMany)(_replaceMany)

  def convertBetween[S, A](_get: S => A)(_reverseGet: A => S): Optic[Get & ReverseGet, S, A] = 
    polyConstructors.convertBetween(_get)(_reverseGet)

  def selectBranch[S, A](_getOption: PartialFunction[S, A])(_reverseGet: A => S): Optic[GetOption & ReverseGet, S, A] =
    POptic(
      new PrismImpl:
        override def reverseGet(a: A): S = _reverseGet(a)
        override def getOrModify(s: S): Either[S, A] = _getOption.lift(s).fold(Left(s))(Right.apply)
        override def getOption(s: S): Option[A] = _getOption.lift(s))

  def traverse[Tr[_]: Traverse, A]: Optic[GetMany & Modify, Tr[A], A] =
    polyConstructors.traverse

  def nonEmptyTraverse[Tr[_]: NonEmptyTraverse, A]: Optic[GetOneOrMore & Modify, Tr[A], A] =
    polyConstructors.nonEmptyTraverse

