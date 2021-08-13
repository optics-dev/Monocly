package monocly.impl

import monocly.internal._
import monocly._

class GetOneImpl[+ThisCan <: GetOne, -S, +A](_get: S => A) extends GetterImpl[ThisCan, S, A]: 

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).map(get))

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetOneOrMoreImpl[ThisCan | ThatCan, S0, A] = 
    GetOneOrMoreImpl(s0 => impl1.getOneOrMore(s0).map(get))

  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetOptionImpl[ThisCan | ThatCan, S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).map(get))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetOneImpl[ThisCan | ThatCan, S0, A] = 
    GetOneImpl(s0 => get(impl1.get(s0)))

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C] = 
    impl2.preComposeGetOne(this)

  override def get(using ThisCan <:< GetOne): S => A = _get
  override def getOption(using ThisCan <:< GetOption): S => Option[A] = s => Some(_get(s))

  override def foldMap1[M: Semigroup](f: A => M)(using ThisCan <:< GetMany): S => M = s => f(_get(s))
  override def getOneOrMore(using ThisCan <:< GetOneOrMore): S => NonEmptyList[A] = s => NonEmptyList(_get(s), Nil)

  override def foldMap[M: Monoid](f: A => M)(using ThisCan <:< GetMany): S => M = s => f(_get(s))
  override def getAll(using ThisCan <:< GetMany): S => List[A] = s => List(_get(s))

end GetOneImpl