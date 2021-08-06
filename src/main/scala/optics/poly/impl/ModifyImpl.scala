package optics.poly


class ModifyImpl[-S, +T, +A, -B](val modify: (A => B) => S => T) extends SetterImpl[Modify, S, T, A, B]:

  override def preComposeModify[S0, T0](impl1: ModifyImpl[S0, T0, S, T]): ModifyImpl[S0, T0, A, B] = 
    ModifyImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  override def preComposeReverseGet[S0, T0](impl1: ReverseGetImpl[S0, T0, S, T]): ModifyImpl[S0, T0, A, B] = 
    ModifyImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  override def andThen[ThatCan <: OpticCan, C, D](impl2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[Modify | ThatCan, S, T, C, D] = 
    impl2.preComposeModify(this)

  override def doModify(f: A => B)(using Modify <:< Modify): S => T = modify(f)

end ModifyImpl