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

type Optic[+ThisCan <: OpticCan, S, A] = PolyOptic[ThisCan, S, S, A, A]

final class PolyOptic[+ThisCan <: OpticCan, S, T, A, B] private[optics](
    protected[optics] val getterImpl: GetterImpl[ThisCan, S, A],
    protected[optics] val setterImpl: SetterImpl[ThisCan, S, T, A, B]):

  def andThen[ThatCan <: OpticCan, AllowedByBoth >: (ThisCan | ThatCan) <: OpticCan, C, D](o: PolyOptic[ThatCan, A, B, C, D]): PolyOptic[AllowedByBoth, S, T, C, D] = 
    PolyOptic(getterImpl.andThen(o.getterImpl), setterImpl.andThen(o.setterImpl))

end PolyOptic


extension [S, T, A, B] (optic: PolyOptic[GetOne, S, T, A, B])
  inline def get: S => A = optic.getterImpl.doGet

extension [S, T, A, B] (optic: PolyOptic[GetOption, S, T, A, B])
  inline def getOption: S => Option[A] = optic.getterImpl.doGetOption

// extension [S, T, A, B] (optic: Optic[GetOneOrMore, S, T, A, B])
//   inline def getOneOrMore: S => NonEmptyList[A] = optic.getterImpl.doGetOneOrMore

extension [S, T, A, B] (optic: PolyOptic[GetMany, S, T, A, B])
  inline def getAll: S => List[A] = optic.getterImpl.doGetAll

extension [S, T, A, B] (optic: PolyOptic[ReverseGet, S, T, A, B])
  inline def reverseGet: B => T = optic.setterImpl.doReverseGet

extension [S, T, A, B] (optic: PolyOptic[Modify, S, T, A, B])
  inline def modify(f: A => B): S => T = optic.setterImpl.doModify(f)

object Optic: 
  def withGetOne[S,A](f: S => A): Optic[GetOne, S, A] = 
    PolyOptic(GetOneImpl(f), NoSetter)

  def withGetOption[S,A](f: S => Option[A]): Optic[GetOption, S, A] = 
    PolyOptic(GetOptionImpl(f), NoSetter)

  // def withGetOneOrMore[S,A](f: S => NonEmptyList[A]): Optic[GetOneOrMore, S, S, A, A] = 
  //   Optic(GetOneOrMoreImpl(f), NoSetter)

  def withGetMany[S,A](f: S => List[A]): Optic[GetMany, S, A] = 
    PolyOptic(GetManyImpl(f), NoSetter)

  def withModify[S,A](f: (A => A) => S => S): Optic[Modify, S, A] = 
    PolyOptic(NoGetter, ModifyImpl(f))

end Optic
