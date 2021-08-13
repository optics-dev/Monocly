package monocly

import monocly.impl._
import monocly.functions.{Each, Index}
import monocly.internal.{NonEmptyList, Applicative, Monoid}


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


final class POptic[+ThisCan <: OpticCan, -S, +T, +A, -B] private[monocly](
    protected[monocly] val getter: GetterImpl[ThisCan, S, A],
    protected[monocly] val setter: SetterImpl[ThisCan, S, T, A, B]):

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

extension [S, T, A, B] (optic: POptic[GetOption & Modify, S, T, A, B])
  inline def getOrModify: S => Either[T, A] = 
    s => optic.getter.getOption(s).fold(Left(???))(Right.apply) 

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
  def some: POptic[ThisCan | GetOption, S, T, A, B] = optic.andThen(std.option.pSome)

extension [ThisCan <: OpticCan, S, A] (optic: Optic[ThisCan, S, Option[A]])
  def withDefault(defaultValue: A): Optic[ThisCan | (GetOne & ReverseGet), S, A] = 
    val iso: Optic[GetOne & ReverseGet, Option[A], A] = POptic(
      GetOneImpl(_.getOrElse(defaultValue)), 
      ReverseGetImpl(f => _.map(f), Some.apply))
    optic.andThen(iso)

object Optic:
  val NullOptic = POptic(NoGetter, NoSetter)

end Optic
