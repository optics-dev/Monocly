package monocly.impl

import monocly._

class GetOptionImpl[+ThisCan <: GetOption, -S, +T, +A](_getOption: S => Option[A], _returnUnmatched: S => T) extends GetterImpl[ThisCan, S, T, A]: 

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getOneOrMore(s0).toList.flatMap(getOption))

  override def preComposeGetOption[ThatCan <: GetOption, S0, T0](impl1: GetOptionImpl[ThatCan, S0, T0, S]): GetOptionImpl[ThisCan | ThatCan, S0, T0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption), impl1.returnUnmatched)

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetOptionImpl[ThisCan | ThatCan, S0, Nothing, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption), impl1.returnUnmatched)

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, _, C]): GetterImpl[ThisCan | ThatCan, S, T, C] = 
    impl2.preComposeGetOption(this)

  override def returnUnmatched(using ThisCan <:< GetOption): S => T = _returnUnmatched  
  override def getOption(using ThisCan <:< GetOption): S => Option[A] = _getOption
  override def getAll(using ThisCan <:< GetMany): S => List[A] = s => _getOption(s).toList

end GetOptionImpl
