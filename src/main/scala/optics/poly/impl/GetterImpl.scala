package optics.poly

import optics.internal.NonEmptyList


trait GetterImpl[+ThisCan <: OpticCan, -S, +T, +A]:

  def canAlso[NewCan <: OpticCan]: GetterImpl[NewCan, S, T, A] = 
    asInstanceOf[GetterImpl[NewCan, S, T, A]]

  def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, Nothing, A]
  def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, Nothing, A]
  def preComposeGetOption[ThatCan <: GetOption, S0, T0](impl1: GetOptionImpl[ThatCan, S0, T0, S]): GetterImpl[ThisCan | ThatCan, S0, T0, A]
  def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetterImpl[ThisCan | ThatCan, S0, Nothing, A]

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, _, C]): GetterImpl[ThisCan | ThatCan, S, T, C]

  def get(using ThisCan <:< GetOne): S => A = sys.error("This optic does not support 'get'")
  def getOption(using ThisCan <:< GetOption): S => Option[A] = sys.error("This optic does not support 'getOption'")
  def returnUnmatched(using ThisCan <:< GetOption): S => T = sys.error("This optic does not support 'returnUnmatched'")
  def getOneOrMore(using ThisCan <:< GetOneOrMore): S => NonEmptyList[A] = sys.error("This optic does not support 'getOneOrMore'")
  def getAll(using ThisCan <:< GetMany): S => List[A] = sys.error("This optic does not support 'getAll'")

end GetterImpl
