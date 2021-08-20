package monocly

import monocly.internal._
import monocly.impl._

object OpticsBuilder:

  extension [SetCan, S, T, A, B](optic: PSetter[SetCan, S, T, A, B])
    def withGetOne(_getOne: S => A): POptic[GetOne, SetCan, S, T, A, B] =
      POptic(GetOneImpl(_getOne), optic.setter)

    def withGetOption(_getOption: S => Option[A]): POptic[GetOption, SetCan, S, T, A, B] =
      POptic(GetOptionImpl(_getOption), optic.setter)

    def withGetOneOrMore(_getOneOrMore: S => NonEmptyList[A]): POptic[GetOneOrMore, SetCan, S, T, A, B] =
      POptic(GetOneOrMoreImpl(_getOneOrMore), optic.setter)

    def withGetMany(_getAll: S => List[A]): POptic[GetMany, SetCan, S, T, A, B] =
      POptic(GetManyImpl(_getAll), optic.setter)

  extension [GetCan, S, T, A, B](optic: POptic[GetCan, Any, S, T, A, B])
    def withModify(_modify: (A => B) => S => T): POptic[GetCan, Modify, S, T, A, B] =
      POptic(optic.getter, ModifyImpl(_modify))

  // extension [ThisCan <: GetOption, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasGetter[ThisCan])
  //   def withReverseGet(_reverseGet: B => T): POptic[ThisCan & ReverseGet, S, T, A, B] = 
  //     POptic(optic.getter.canAlso[ThisCan & ReverseGet], 
  //            ReverseGetImpl(f => s => optic.getter.getOption(s).fold(optic.getter.returnUnmatched(s))
  //                                                                   (_reverseGet.compose(f)), _reverseGet))

  def withGetOne[S, A](_get: S => A): Getter[GetOne, S, A] =
    POptic(GetOneImpl(_get), NoSetter)

  def withGetOption[S, A](_getOption: S => Option[A]): Getter[GetOption, S, A] =
    POptic(GetOptionImpl(_getOption), NoSetter)

  def withGetOneOrMore[S, A](_getOneOrMore: S => NonEmptyList[A]): Getter[GetOneOrMore, S, A] =
    POptic(GetOneOrMoreImpl(_getOneOrMore), NoSetter)

  def withGetMany[S,A](_getList: S => List[A]): Getter[GetMany, S, A] =
    POptic(GetManyImpl(_getList), NoSetter)

  def withModify[S, T, A, B](_modify: (A => B) => S => T): PSetter[Modify, S, T, A, B] =
    POptic(NoGetter, ModifyImpl(_modify))

  def withReverseGet[S, T, A, B](_modify: (A => B) => S => T, _reverseGet: B => T): PSetter[ReverseGet, S, T, A, B] =
    POptic(NoGetter, ReverseGetImpl(_modify, _reverseGet))

  def withLens[S, A](_get: S => A)(_set: A => S => S): Optic[GetOne, Modify, S, A] =
    Lens(_get, _set)

end OpticsBuilder