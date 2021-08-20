package monocly.impl

import monocly.internal._
import monocly._

object GetOneOrMoreImpl:
  def apply[ThisCan <: GetOneOrMore, S, A](_getOneOrMore: S => NonEmptyList[A]): GetOneOrMoreImpl[ThisCan, S, A] = new GetOneOrMoreImpl:
    override def _foldMap1[M: Semigroup](f: A => M): S => M = 
      s => 
        val NonEmptyList(head, tail) = _getOneOrMore(s)
        tail.foldLeft(f(head))((m, a) => Semigroup[M].combine(m, f(a)))

trait GetOneOrMoreImpl[+ThisCan <: GetOneOrMore, -S, +A] extends GetterImpl[ThisCan, S, A]: 
  self => 

  protected def _foldMap1[M: Semigroup](f: A => M): S => M

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => impl1.foldMap(s => self.foldMap1(f)(s))(s0)

  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetOneOrMoreImpl[ThisCan | ThatCan, S0, A] = 
    new GetOneOrMoreImpl:
      override def _foldMap1[M: Semigroup](f: A => M): S0 => M = 
        s0 => impl1.foldMap1(s => self.foldMap1(f)(s))(s0)

  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    new GetManyImpl:
      override def _foldMap[M: Monoid](f: A => M): S0 => M = 
        s0 => impl1.getOption(s0).fold(Monoid[M].empty)(self.foldMap1(f))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetOneOrMoreImpl[ThisCan | ThatCan, S0, A] = 
    new GetOneOrMoreImpl:
      override def _foldMap1[M: Semigroup](f: A => M): S0 => M = 
        s0 => self.foldMap1(f)(impl1.get(s0))

  override def andThen[ThatCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C] =
    impl2.preComposeGetOneOrMore(this)

  override def foldMap1[M](f: A => M)(using Semigroup[M], ThisCan <:< GetOneOrMore): S => M = _foldMap1(f)
  override def foldMap[M](f: A => M)(using Monoid[M], ThisCan <:< GetMany): S => M = _foldMap1(f)

end GetOneOrMoreImpl
