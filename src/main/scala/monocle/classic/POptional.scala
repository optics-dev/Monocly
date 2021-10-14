package monocle.classic

import monocle.*
import monocle.internal.{Applicative, Id}
import monocle.functions.Index
import monocle.impl._

type POptional[-S, +T, +A, -B] = POptic[GetOption & Modify, S, T, A, B]
type Optional[S, A]            = POptional[S, S, A, A]

object POptional:
  def apply[S, T, A, B](_getOrModify: S => Either[T, A], _replace: B => S => T): POptional[S, T, A, B] =
    POptic.thatCan.editOption(_getOrModify)(_replace)

object Optional:
  def apply[From, To](_getOption: From => Option[To], _replace: To => From => From): Optional[From, To] =
    POptional(from => _getOption(from).toRight(from), _replace)
