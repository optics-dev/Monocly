package monocly.impl

import monocly._
import monocly.internal._

class GetOptionImpl[+ThisCan <: GetOption, -S, +A](_getOption: S => Option[A]) extends GetterImpl[ThisCan, S, A]: 
  self => 

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => impl1.foldMap(s => self.getOption(s).fold(Monoid[M].empty)(f))(s0)

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => impl1.foldMap1(s => self.getOption(s).fold(Monoid[M].empty)(f))(s0)

  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetOptionImpl[ThisCan | ThatCan, S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetOptionImpl[ThisCan | ThatCan, S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C] = 
    impl2.preComposeGetOption(this)

  override def getOption(using ThisCan <:< GetOption): S => Option[A] = _getOption

  override def foldMap[M](f: A => M)(using Monoid[M], ThisCan <:< GetMany): S => M = 
    s => _getOption(s).fold(Monoid[M].empty)(f)
    
end GetOptionImpl
