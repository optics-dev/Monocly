package monocly

import monocly.impl._
import monocly.functions.{Each, Index}
import monocly.internal.{NonEmptyList, Applicative, Monoid}

type Getter[+GetCan, S, A] = Optic[GetCan, Any, S, A]
type Setter[+SetCan, S, A] = Optic[Any, SetCan, S, A]
type PSetter[+SetCan, -S, +T, +A, -B] = POptic[Any, SetCan, S, T, A, B]
type Optic[+GetCan, +SetCan, S, A] = POptic[GetCan, SetCan, S, S, A, A]

final class POptic[+GetCan, +SetCan, -S, +T, +A, -B] private[monocly](
    protected[monocly] val getter: GetterImpl[GetCan, S, A],
    protected[monocly] val setter: SetterImpl[SetCan, S, T, A, B]):

  def andThen[ThatGetCan >: GetCan, ThatSetCan >: SetCan, C, D](
   o: POptic[ThatGetCan, ThatSetCan, A, B, C, D]
  ): POptic[ThatGetCan, ThatSetCan, S, T, C, D] =
    POptic(getter.andThen(o.getter), setter.andThen(o.setter))

end POptic


extension [GetCan, SetCan, S, A] (optic: Optic[GetCan, SetCan, S, A])
  def each[C](using evEach: Each[A, C]): Optic[GetCan | GetMany, SetCan | Modify, S, C] =
    optic.andThen(evEach.each)

  def index[I, A1](i: I)(using evIndex: Index[A, I, A1]): Optic[GetCan | GetOption, SetCan | Modify, S, A1] =
    optic.andThen(evIndex.index(i))

extension [S, T, A, B] (optic: POptic[GetMany, Any, S, T, A, B])
  inline def foldMap[M: Monoid](f: A => M)(s: S): M = ???

extension [S, T, A, B] (optic: POptic[GetOne, Any, S, T, A, B])
  inline def get: S => A = optic.getter.get

extension [S, T, A, B] (optic: POptic[GetMany, Any, S, T, A, B])
  inline def getAll: S => List[A] = optic.getter.foldMap[List[A]](List(_))

extension [S, T, A, B] (optic: POptic[GetOneOrMore, Any, S, T, A, B])
  inline def getOneOrMore: S => NonEmptyList[A] = optic.getter.foldMap1[NonEmptyList[A]](NonEmptyList(_, Nil))

extension [S, T, A, B] (optic: POptic[GetOption, Any, S, T, A, B])
  inline def getOption: S => Option[A] = optic.getter.getOption

extension [S, T, A, B] (optic: POptic[GetOption, Modify, S, T, A, B])
  inline def getOrModify: S => Either[T, A] = 
    s => optic.getter.getOption(s).fold(Left(???))(Right.apply)

extension [S, T, A, B] (optic: POptic[Any, Modify, S, T, A, B])
  inline def modify(f: A => B): S => T = optic.setter.modify(f)

extension [S, T, A, B] (optic: POptic[GetMany, Modify, S, T, A, B])
  inline def modifyA[F[_]: Applicative](f: A => F[B])(s: S): F[T] = ???

extension [S, T, A, B] (optic: POptic[Any, Modify, S, T, A, B])
  inline def replace(b: B): S => T = optic.setter.modify(_ => b)
  
extension [S, T, A, B] (optic: POptic[Any, ReverseGet, S, T, A, B])
  inline def reverseGet: B => T = optic.setter.reverseGet

extension [GetCan, SetCan, S, T, A, B] (optic: POptic[GetCan, SetCan, S, T, Option[A], Option[B]])
  def some: POptic[GetCan | GetOption, SetCan | ReverseGet, S, T, A, B] = optic.andThen(std.option.pSome)

extension [GetCan, SetCan, S, A] (optic: Optic[GetCan, SetCan, S, Option[A]])
  def withDefault(defaultValue: A): Optic[GetCan | GetOne, SetCan | ReverseGet, S, A] =
    val iso: Optic[GetOne, ReverseGet, Option[A], A] = POptic(
      GetOneImpl(_.getOrElse(defaultValue)), 
      ReverseGetImpl(f => _.map(f), Some.apply))
    optic.andThen(iso)

object Optic:
  val NullOptic = POptic(NoGetter, NoSetter)

end Optic
