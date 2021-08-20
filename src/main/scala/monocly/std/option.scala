package monocly.std

import monocly._
import monocly.impl._

object option:
  def pSome[A, B]: POptic[GetOption, ReverseGet, Option[A], Option[B], A, B] =
    POptic(GetOptionImpl(identity), ReverseGetImpl(f => _.map(f), Some.apply))

  def some[A]: Optic[GetOption, ReverseGet, Option[A], A] =
    pSome[A, A]