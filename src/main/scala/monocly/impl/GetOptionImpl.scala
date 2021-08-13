package monocly.impl

import monocly._

class GetOptionImpl[+ThisCan <: GetOption, -S, +A](_getOption: S => Option[A]) extends GetterImpl[ThisCan, S, A]: 

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getOneOrMore(s0).toList.flatMap(getOption))

  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetOptionImpl[ThisCan | ThatCan, S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetOptionImpl[ThisCan | ThatCan, S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))


  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C] = 
    impl2.preComposeGetOption(this)


  override def getOption(using ThisCan <:< GetOption): S => Option[A] = _getOption
  override def getAll(using ThisCan <:< GetMany): S => List[A] = s => getOption(s).toList

end GetOptionImpl
