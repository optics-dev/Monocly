package optics.poly


object NoSetter extends SetterImpl[Nothing, Any, Nothing, Nothing, Any]:
  override def preComposeModify[S0, T0](impl1: ModifyImpl[S0, T0, Any, Nothing]) = NoSetter
  override def preComposeReverseGet[S0, T0](impl1: ReverseGetImpl[S0, T0, Any, Nothing]) = NoSetter
  override def andThen[ThatCan <: OpticCan, C, D](impl2: SetterImpl[ThatCan, Nothing, Any, C, D]) = NoSetter
end NoSetter