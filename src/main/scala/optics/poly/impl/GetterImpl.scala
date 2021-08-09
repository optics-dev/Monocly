package optics.poly

import optics.internal.NonEmptyList


trait GetterImpl[+ThisCan <: OpticCan, -S, +A]:

  def canAlso[NewCan <: OpticCan]: GetterImpl[NewCan, S, A] = 
    asInstanceOf[GetterImpl[NewCan, S, A]]

  def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0,S]): GetterImpl[ThisCan | ThatCan, S0, A]
  def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, A]
  def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0,S]): GetterImpl[ThisCan | ThatCan, S0, A]
  def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0,S]): GetterImpl[ThisCan | ThatCan, S0, A]

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A,C]): GetterImpl[ThisCan | ThatCan, S, C]

  def get(using ThisCan <:< GetOne): S => A = sys.error("This optic does not support 'get'")
  def getOption(using ThisCan <:< GetOption): S => Option[A] = sys.error("This optic does not support 'getOption'")
  def getOneOrMore(using ThisCan <:< GetOneOrMore): S => NonEmptyList[A] = sys.error("This optic does not support 'getOneOrMore'")
  def getAll(using ThisCan <:< GetMany): S => List[A] = sys.error("This optic does not support 'getAll'")

end GetterImpl
