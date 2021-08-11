package optics.poly

import optics.internal.NonEmptyList

class GetOneOrMoreImpl[+ThisCan <: GetOneOrMore, -S, +A](_getOneOrMore: S => NonEmptyList[A]) extends GetterImpl[ThisCan, S, Nothing, A]: 

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetOneOrMoreImpl[ThisCan | ThatCan, S0, A] = 
    GetOneOrMoreImpl(s0 => impl1.getOneOrMore(s0).flatMap(getOneOrMore))

  override def preComposeGetOption[ThatCan <: GetOption, S0, T0](impl1: GetOptionImpl[ThatCan, S0, T0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getOption(s0).fold(Nil)(s => getOneOrMore(s).toList))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetOneOrMoreImpl[ThisCan | ThatCan, S0, A] = 
    GetOneOrMoreImpl(s0 => getOneOrMore(impl1.get(s0)))

  override def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, _, C]): GetterImpl[ThisCan | ThatCan, S, Nothing, C] = 
    impl2.preComposeGetOneOrMore(this)

  override def getOneOrMore(using ThisCan <:< GetOneOrMore): S => NonEmptyList[A] = _getOneOrMore
  override def getAll(using ThisCan <:< GetMany): S => List[A] = s => _getOneOrMore(s).toList

end GetOneOrMoreImpl
