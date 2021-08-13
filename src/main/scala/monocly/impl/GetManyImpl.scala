package monocly.impl

import monocly._
import monocly.internal._

class GetManyImpl[+ThisCan <: GetMany, -S, +A](_getAll: S => List[A]) extends GetterImpl[ThisCan, S, A]: 

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0,S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getOneOrMore(s0).toList.flatMap(getAll))

  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0,S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => getAll(impl1.get(s0)))

  override def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C] = 
    impl2.preComposeGetMany(this)

  override def foldMap[M: Monoid](f: A => M)(using ThisCan <:< GetMany): S => M = 
    s => _getAll(s).map(f).foldLeft(Monoid[M].empty)(Monoid[M].combine)

  override def getAll(using ThisCan <:< GetMany): S => List[A] = _getAll

end GetManyImpl