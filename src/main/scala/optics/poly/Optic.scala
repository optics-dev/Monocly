package optics.poly

import optics.poly.functions.{Each, Index}
import optics.internal.{NonEmptyList, Applicative, Monoid}

// type Getter[-S, +A]             = POptic[GetOne, S, Nothing, A, Any]
// type Fold[-S, +A]               = POptic[GetMany, S, Nothing, A, Any]
// type PSetter[-S, +T, +A, -B]    = POptic[Modify, S, T, A, B]
// type PTraversal[-S, +T, +A, -B] = POptic[GetMany & Modify, S, T, A, B]
// type POptional[-S, +T, +A, -B]  = POptic[GetOption & Modify, S, T, A, B]
// type PPrism[-S, +T, +A, -B]     = POptic[GetOption & ReverseGet, S, T, A, B]
// type PLens[-S, +T, +A, -B]      = POptic[GetOne & Modify, S, T, A, B]
// type PIso[-S, +T, +A, -B]       = POptic[GetOne & ReverseGet, S, T, A, B]

type Traversal[From, To] = PTraversal[From, From, To, To]
type Optional[From, To]  = POptional[From, From, To, To]
type Prism[From, To]     = PPrism[From, From, To, To]
type Lens[From, To]      = PLens[From, From, To, To]
type Iso[From, To]       = PIso[From, From, To, To]



sealed trait OpticCan

//        GetMany
//          /\
//         /  \
// GetOption  GetOneOrMore
//         \  /
//          \/
//        GetOne
trait GetMany extends OpticCan
trait GetOption extends GetMany
trait GetOneOrMore extends GetMany
trait GetOne extends GetOption with GetOneOrMore 

//    Modify
//      ^
//      |
//  ReverseGet
trait Modify extends OpticCan
trait ReverseGet extends Modify

type Optic[+ThisCan <: OpticCan, S, A] = POptic[ThisCan, S, S, A, A]


final class POptic[+ThisCan <: OpticCan, -S, +T, +A, -B] private[optics](
    protected[optics] val getter: GetterImpl[ThisCan, S, T, A],
    protected[optics] val setter: SetterImpl[ThisCan, S, T, A, B]):

  def andThen[ThatCan <: OpticCan, BothCan >: (ThisCan | ThatCan) <: OpticCan, C, D](o: POptic[ThatCan, A, B, C, D]): POptic[BothCan, S, T, C, D] = 
    POptic(getter.andThen(o.getter), setter.andThen(o.setter))

end POptic


extension [ThisCan <: OpticCan, S, A] (optic: Optic[ThisCan, S, A])
  def each[C](using evEach: Each[A, C]): Optic[ThisCan | (GetMany & Modify), S, C] = 
    optic.andThen(evEach.each)

extension [S, T, A, B] (optic: POptic[GetMany, S, T, A, B])
  inline def foldMap[M: Monoid](f: A => M)(s: S): M = ???

extension [S, T, A, B] (optic: POptic[GetOne, S, T, A, B])
  inline def get: S => A = optic.getter.get

extension [S, T, A, B] (optic: POptic[GetMany, S, T, A, B])
  inline def getAll: S => List[A] = optic.getter.getAll

extension [S, T, A, B] (optic: POptic[GetOneOrMore, S, T, A, B])
  inline def getOneOrMore: S => NonEmptyList[A] = optic.getter.getOneOrMore

extension [S, T, A, B] (optic: POptic[GetOption, S, T, A, B])
  inline def getOption: S => Option[A] = optic.getter.getOption

extension [S, T, A, B] (optic: POptic[GetOption, S, T, A, B])
  inline def getOrModify: S => Either[T, A] = 
    s => optic.getter.getOption(s).fold(Left(optic.getter.returnUnmatched(s)))(Right.apply) 

extension [ThisCan <: OpticCan, S, A] (optic: Optic[ThisCan, S, A])
  def index[I, A1](i: I)(using evIndex: Index[A, I, A1]): Optic[ThisCan | (GetOption & Modify), S, A1] = 
    optic.andThen(evIndex.index(i))

extension [S, T, A, B] (optic: POptic[Modify, S, T, A, B])
  inline def modify(f: A => B): S => T = optic.setter.modify(f)

extension [S, T, A, B] (optic: POptic[GetMany & Modify, S, T, A, B])
  inline def modifyA[F[_]: Applicative](f: A => F[B])(s: S): F[T] = ???

extension [S, T, A, B] (optic: POptic[Modify, S, T, A, B])
  inline def replace(b: B): S => T = optic.setter.modify(_ => b)
  
extension [S, T, A, B] (optic: POptic[ReverseGet, S, T, A, B])
  inline def reverseGet: B => T = optic.setter.reverseGet

extension [ThisCan <: OpticCan, S, T, A, B] (optic: POptic[ThisCan, S, T, Option[A], Option[B]])
  def some: POptic[ThisCan | GetOption, S, T, A, B] = 
    val prism: POptic[GetOption & ReverseGet, Option[A], Option[B], A, B] = 
      POptic(GetOptionImpl(identity, _ => None), ReverseGetImpl(f => _.map(f), Some.apply))
    optic.andThen(prism)

extension [ThisCan <: OpticCan, S, A] (optic: Optic[ThisCan, S, Option[A]])
  def withDefault(defaultValue: A): Optic[ThisCan | (GetOne & ReverseGet), S, A] = 
    import OpticsBuilder._
    val iso = OpticsBuilder.withGetOne[Option[A], A](_.getOrElse(defaultValue)).withReverseGet(Some.apply)
    optic.andThen(iso)

object Optic:
  val NullOptic = POptic(NoGetter, NoSetter)

end Optic
