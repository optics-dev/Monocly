package monocly.impl

import monocly._
import monocly.internal._

object GetManyImpl:
  def apply[ThisCan <: GetMany, S, A](_getAll: S => List[A]): GetManyImpl[ThisCan, S, A] = new GetManyImpl:
    override def _foldMap[M: Monoid](f: A => M): S => M = 
      s => _getAll(s).foldLeft(Monoid[M].empty)((m, a) => Monoid[M].combine(m, f(a)))

trait GetManyImpl[+ThisCan <: GetMany, -S, +A] extends GetterImpl[ThisCan, S, A]: 
  self => 

  protected def _foldMap[M: Monoid](f: A => M): S => M

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => impl1.foldMap(s => self.foldMap(f)(s))(s0)

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => impl1.foldMap1(s => self.foldMap(f)(s))(s0)

  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => impl1.getOption(s0).fold(Monoid[M].empty)(self.foldMap(f))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => self.foldMap(f)(impl1.get(s0))

  override def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C] = 
    impl2.preComposeGetMany(this)

  override def foldMap[M](f: A => M)(using Monoid[M], ThisCan <:< GetMany): S => M = _foldMap(f)


end GetManyImpl