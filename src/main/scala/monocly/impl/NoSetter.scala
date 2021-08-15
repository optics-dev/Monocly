package monocly.impl

import monocly._

object NoSetter extends SetterImpl[Nothing, Any, Nothing, Nothing, Any]:
  override def preComposeModify[ThatCan <: Modify, S0, T0](impl1: ModifyImpl[ThatCan, S0, T0, Any, Nothing]) = NoSetter
  override def preComposeReverseGet[ThatCan <: ReverseGet, S0, T0](impl1: ReverseGetImpl[ThatCan, S0, T0, Any, Nothing]) = NoSetter
  override def andThen[ThatCan <: OpticCan, C, D](impl2: SetterImpl[ThatCan, Nothing, Any, C, D]) = NoSetter
end NoSetter