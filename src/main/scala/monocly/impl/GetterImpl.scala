package monocly.impl

import monocly.internal._
import monocly._


trait GetterImpl[+ThisCan <: OpticCan, -S, +A]:

  def canAlso[NewCan <: OpticCan]: GetterImpl[NewCan, S, A] = 
    asInstanceOf[GetterImpl[NewCan, S, A]]

  def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, A]
  def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, A]
  def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, A]
  def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, A]

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C]

  def get(using ThisCan <:< GetOne): S => A = sys.error("This optic does not support 'get'")
  def getOption(using ThisCan <:< GetOption): S => Option[A] = sys.error("This optic does not support 'getOption'")
  def foldMap1[M](f: A => M)(using Semigroup[M], ThisCan <:< GetOneOrMore): S => M = sys.error("This optic does not support 'foldMap1'")
  def foldMap[M](f: A => M)(using Monoid[M], ThisCan <:< GetMany): S => M = sys.error("This optic does not support 'foldMap'")


end GetterImpl
