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
trait GetOneOrMore extends GetMany
trait GetOne extends GetOption with GetOneOrMore 
trait Modify extends OpticCan
trait ReverseGet extends Modify

type Optic[+ThisCan <: OpticCan, S, A] = POptic[ThisCan, S, S, A, A]


final class POptic[+ThisCan <: OpticCan, -S, +T, +A, -B] private[optics](
    protected[optics] val getter: GetterImpl[ThisCan, S, A],
    protected[optics] val setter: SetterImpl[ThisCan, S, T, A, B]):

  def andThen[ThatCan <: OpticCan, BothCan >: (ThisCan | ThatCan) <: OpticCan, C, D](o: POptic[ThatCan, A, B, C, D]): POptic[BothCan, S, T, C, D] = 
    POptic(getter.andThen(o.getter), setter.andThen(o.setter))

end POptic


extension [S, T, A, B] (optic: POptic[GetOne, S, T, A, B])
  inline def get: S => A = optic.getter.get

extension [S, T, A, B] (optic: POptic[GetOption, S, T, A, B])
  inline def getOption: S => Option[A] = optic.getter.getOption

extension [S, T, A, B] (optic: POptic[GetOneOrMore, S, T, A, B])
  inline def getOneOrMore: S => NonEmptyList[A] = optic.getter.getOneOrMore

extension [S, T, A, B] (optic: POptic[GetMany, S, T, A, B])
  inline def getAll: S => List[A] = optic.getter.getAll

extension [S, T, A, B] (optic: POptic[ReverseGet, S, T, A, B])
  inline def reverseGet: B => T = optic.setter.reverseGet

extension [S, T, A, B] (optic: POptic[Modify, S, T, A, B])
  inline def modify(f: A => B): S => T = optic.setter.modify(f)

extension [S, T, A, B] (optic: POptic[Modify, S, T, A, B])
  inline def replace(b: B): S => T = optic.setter.modify(_ => b)


object Optic:
  def NullOptic[S, T, A, B] = POptic(NoGetter, NoSetter)

end Optic
