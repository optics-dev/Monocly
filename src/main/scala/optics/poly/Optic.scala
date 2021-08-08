package optics.poly

import optics.internal.NonEmptyList

// type PTraversal[S, T, A, B] = Optic[GetMany & Modify, S, T, A, B]
// type POptional[S, T, A, B]  = Optic[GetOption & Modify, S, T, A, B]
// type PPrism[S, T, A, B]     = Optic[GetOption & ReverseGet, S, T, A, B]
// type PLens[S, T, A, B]      = Optic[GetOne & Modify, S, T, A, B]
// type PIso[S, T, A, B]       = Optic[GetOne & ReverseGet, S, T, A, B]

type Traversal[From, To] = PTraversal[From, From, To, To]
type Optional[From, To]  = POptional[From, From, To, To]
type Prism[From, To]     = PPrism[From, From, To, To]
type Lens[From, To]      = PLens[From, From, To, To]
type Iso[From, To]       = PIso[From, From, To, To]


sealed trait OpticCan
trait GetMany extends OpticCan
trait GetOption extends GetMany
trait GetOneOrMore extends OpticCan
trait GetOne extends GetOption //with GetOneOrMore 
trait Modify extends OpticCan
trait ReverseGet extends Modify

type Optic[+ThisCan <: OpticCan, S, A] = POptic[ThisCan, S, S, A, A]

final class POptic[+ThisCan <: OpticCan, S, T, A, B] private[optics](
    protected[optics] val getter: GetterImpl[ThisCan, S, A],
    protected[optics] val setter: SetterImpl[ThisCan, S, T, A, B]):

  def andThen[ThatCan <: OpticCan, AllowedByBoth >: (ThisCan | ThatCan) <: OpticCan, C, D](o: POptic[ThatCan, A, B, C, D]): POptic[AllowedByBoth, S, T, C, D] = 
    POptic(getter.andThen(o.getter), setter.andThen(o.setter))


end POptic


extension [S, T, A, B] (optic: POptic[GetOne, S, T, A, B])
  inline def get: S => A = optic.getter.doGet

extension [S, T, A, B] (optic: POptic[GetOption, S, T, A, B])
  inline def getOption: S => Option[A] = optic.getter.doGetOption

// extension [S, T, A, B] (optic: Optic[GetOneOrMore, S, T, A, B])
//   inline def getOneOrMore: S => NonEmptyList[A] = optic.getterImpl.doGetOneOrMore

extension [S, T, A, B] (optic: POptic[GetMany, S, T, A, B])
  inline def getAll: S => List[A] = optic.getter.doGetAll

extension [S, T, A, B] (optic: POptic[ReverseGet, S, T, A, B])
  inline def reverseGet: B => T = optic.setter.doReverseGet

extension [S, T, A, B] (optic: POptic[Modify, S, T, A, B])
  inline def modify(f: A => B): S => T = optic.setter.doModify(f)

extension [S, T, A, B] (optic: POptic[Modify, S, T, A, B])
  inline def replace(b: B): S => T = optic.setter.doModify(_ => b)

type OnlyHasSetter[Can <: OpticCan] = ReverseGet <:< Can
type OnlyHasGetter[Can <: OpticCan] = GetOne <:< Can

extension [ThisCan <: OpticCan, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasSetter[ThisCan])
  def withGetOne(f: S => A): POptic[ThisCan & GetOne, S, T, A, B] = 
    POptic(GetOneImpl(f), optic.setter.cast[ThisCan & GetOne])

extension [ThisCan <: OpticCan, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasSetter[ThisCan])
  def withGetOption(f: S => Option[A]): POptic[ThisCan & GetOption, S, T, A, B] = 
    POptic(GetOptionImpl(f), optic.setter.cast[ThisCan & GetOption])

// def withGetOneOrMore[S,A](f: S => NonEmptyList[A]): Optic[ThisCan & GetOneOrMore, S, S, A, A] = 
//   Optic(GetOneOrMoreImpl(f), setter.cast[ThisCan & GetOneOrMore])

extension [ThisCan <: OpticCan, S, T, A, B](optic: POptic[ThisCan, S, T, A, B])(using OnlyHasSetter[ThisCan])
  def withGetMany(f: S => List[A]): POptic[ThisCan & GetMany, S, T, A, B] = 
    POptic(GetManyImpl(f), optic.setter.cast[ThisCan & GetMany])

extension [ThisCan <: OpticCan, S, T, A, B](optic: Optic[ThisCan, S, A])(using OnlyHasGetter[ThisCan])
  def withModify(f: (A => B) => S => T): POptic[ThisCan & Modify, S, T, A, B] = 
    POptic(optic.getter.cast[ThisCan & Modify], ModifyImpl(f))

extension [ThisCan <: OpticCan, S, T, A, B](optic: Optic[ThisCan, S, A])(using OnlyHasGetter[ThisCan])
  def withReverseGet(modify: (A => B) => S => T, f: B => T): POptic[ThisCan & ReverseGet, S, T, A, B] = 
    POptic(optic.getter.cast[ThisCan & ReverseGet], ReverseGetImpl(modify, f))


object Optic:
  val NullOptic = POptic(NoGetter, NoSetter)

  def withGetOne[S, A](_get: S => A): Optic[GetOne, S, A] = 
    POptic(GetOneImpl(_get), NoSetter)

  def withGetOption[S, A](_getOption: S => Option[A]): Optic[GetOption, S, A] = 
    POptic(GetOptionImpl(_getOption), NoSetter)

  // def withGetOneOrMore[S,A](f: S => NonEmptyList[A]): Optic[GetOneOrMore, S, S, A, A] = 
  //   Optic(GetOneOrMoreImpl(f), NoSetter)

  def withGetMany[S,A](_getList: S => List[A]): Optic[GetMany, S, A] = 
    POptic(GetManyImpl(_getList), NoSetter)

  def withModify[S, T, A, B](_modify: (A => B) => S => T): POptic[Modify, S, T, A, B] = 
    POptic(NoGetter, ModifyImpl(_modify))

  def withReverseGet[S, T, A, B](_modify: (A => B) => S => T, _reverseGet: B => T): POptic[ReverseGet, S, T, A, B] = 
    POptic(NoGetter, ReverseGetImpl(_modify, _reverseGet))

  def withLens[S, A](_get: S => A)(_set: A => S => S): Optic[GetOne & Modify, S, A] = 
    withGetOne(_get).withModify(f => s => _set(f(_get(s)))(s))

end Optic
