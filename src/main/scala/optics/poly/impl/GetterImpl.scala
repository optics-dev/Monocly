package optics.poly

import optics.internal.NonEmptyList

trait GetterImpl[+ThisCan <: OpticCan, -S, +A]:

  def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetterImpl[ThisCan | GetMany, S0, A]
  //def preComposeGetOneOrMore[AllowedByBoth >: (ThisCan | GetOneOrMore) <: OpticCan, S0](impl1: GetOneOrMoreImpl[S0,S]): GetterImpl[AllowedByBoth, S0, A]
  def preComposeGetOption[AllowedByBoth >: (ThisCan | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0,S]): GetterImpl[AllowedByBoth, S0, A]
  def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetterImpl[ThisCan | GetOne, S0, A]

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A,C]): GetterImpl[ThisCan | ThatCan, S, C]

  def doGet(using ThisCan <:< GetOne): S => A = sys.error("This optic does not support 'get'")
  def doGetOption(using ThisCan <:< GetOption): S => Option[A] = sys.error("This optic does not support 'getOption'")
  //def doGetOneOrMore(using ThisCan <:< GetOneOrMore): S => NonEmptyList[A] = sys.error("This optic does not support 'getOneOrMore'")
  def doGetAll(using ThisCan <:< GetMany): S => List[A] = sys.error("This optic does not support 'getAll'")

end GetterImpl
