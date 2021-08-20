package monocly.impl

import monocly._

trait SetterImpl[+ThisCan, -S, +T, +A, -B]:

  def canAlso[NewCan <: SetterCan]: SetterImpl[NewCan, S, T, A, B] =
    asInstanceOf[SetterImpl[NewCan, S, T, A, B]]

  def preComposeModify[ThatCan <: Modify, S0, T0](impl1: ModifyImpl[ThatCan, S0, T0, S, T]): SetterImpl[ThisCan | ThatCan, S0, T0, A, B]
  def preComposeReverseGet[ThatCan <: ReverseGet, S0, T0](impl1: ReverseGetImpl[ThatCan, S0, T0, S, T]): SetterImpl[ThisCan | ThatCan, S0, T0, A, B]

  def andThen[ThatCan, C, D](impl2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[ThisCan | ThatCan, S, T, C, D]

  def modify(f: A => B)(using ThisCan <:< Modify): S => T = sys.error("This optic does not support 'modify'")
  def reverseGet(using ThisCan <:< ReverseGet): B => T = sys.error("This optic does not support 'replaceAll'")

end SetterImpl