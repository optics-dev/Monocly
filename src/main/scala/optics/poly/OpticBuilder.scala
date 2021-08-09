package optics.poly

import optics.internal.NonEmptyList

object OpticsBuilder:
  type OnlyHasSetter[Can <: OpticCan] = ReverseGet <:< Can
  type OnlyHasGetter[Can <: OpticCan] = GetOne <:< Can

  extension [ThisCan <: Modify, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasSetter[ThisCan])
    def withGetOne(f: S => A): POptic[ThisCan & GetOne, S, T, A, B] = 
      POptic(GetOneImpl(f), optic.setter.canAlso[ThisCan & GetOne])

  extension [ThisCan <: Modify, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasSetter[ThisCan])
    def withGetOption(f: S => Option[A]): POptic[ThisCan & GetOption, S, T, A, B] = 
      POptic(GetOptionImpl(f), optic.setter.canAlso[ThisCan & GetOption])

  extension [ThisCan <: Modify, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasSetter[ThisCan])
    def withGetOneOrMore(f: S => NonEmptyList[A]): POptic[ThisCan & GetOneOrMore, S, T, A, B] = 
      POptic(GetOneOrMoreImpl(f), optic.setter.canAlso[ThisCan & GetOneOrMore])

  extension [ThisCan <: Modify, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasSetter[ThisCan])
    def withGetMany(f: S => List[A]): POptic[ThisCan & GetMany, S, T, A, B] = 
      POptic(GetManyImpl(f), optic.setter.canAlso[ThisCan & GetMany])

  extension [ThisCan <: GetMany, S, T, A, B](optic: Optic[ThisCan, S, A])(using OnlyHasGetter[ThisCan])
    def withModify(_modify: (A => B) => S => T): POptic[ThisCan & Modify, S, T, A, B] = 
      POptic(optic.getter.canAlso[ThisCan & Modify], ModifyImpl(_modify))

  extension [ThisCan <: GetOption, S, T, A, B](optic: Optic[ThisCan, S, A])(using OnlyHasGetter[ThisCan])
    def withReverseGet(_modify: (A => B) => S => T, _reverseGet: B => T): POptic[ThisCan & ReverseGet, S, T, A, B] = 
      POptic(optic.getter.canAlso[ThisCan & ReverseGet], ReverseGetImpl(_modify, _reverseGet))

  def withGetOne[S, A](_get: S => A): Optic[GetOne, S, A] = 
    POptic(GetOneImpl(_get), NoSetter)

  def withGetOption[S, A](_getOption: S => Option[A]): Optic[GetOption, S, A] = 
    POptic(GetOptionImpl(_getOption), NoSetter)

  def withGetOneOrMore[S, A](_getOneOrMore: S => NonEmptyList[A]): Optic[GetOneOrMore, S, A] = 
    POptic(GetOneOrMoreImpl(_getOneOrMore), NoSetter)

  def withGetMany[S,A](_getList: S => List[A]): Optic[GetMany, S, A] = 
    POptic(GetManyImpl(_getList), NoSetter)

  def withModify[S, T, A, B](_modify: (A => B) => S => T): POptic[Modify, S, T, A, B] = 
    POptic(NoGetter, ModifyImpl(_modify))

  def withReverseGet[S, T, A, B](_modify: (A => B) => S => T, _reverseGet: B => T): POptic[ReverseGet, S, T, A, B] = 
    POptic(NoGetter, ReverseGetImpl(_modify, _reverseGet))

  def withLens[S, A](_get: S => A)(_set: A => S => S): Optic[GetOne & Modify, S, A] = 
    withGetOne(_get).withModify(f => s => _set(f(_get(s)))(s))

end OpticsBuilder