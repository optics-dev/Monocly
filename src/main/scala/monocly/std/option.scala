package monocly.std

import monocly.*
import monocly.impl.*
import OpticCan.*

object option:
  def pSome[A, B]: POptic[GetOption & ReverseGet, Option[A], Option[B], A, B] =
    POptic.thatCan.selectBranch[Option[A], Option[B], A, B](_.map(Right(_)) getOrElse Left(None))(Some.apply)

  def some[A]: Optic[GetOption & ReverseGet, Option[A], A] =
    pSome[A, A]