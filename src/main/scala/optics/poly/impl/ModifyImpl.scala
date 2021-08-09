package optics.poly


class ModifyImpl[ThisCan <: Modify, -S, +T, +A, -B](_modify: (A => B) => S => T) extends SetterImpl[ThisCan, S, T, A, B]:

  override def preComposeModify[ThatCan <: Modify, S0, T0](impl1: ModifyImpl[ThatCan, S0, T0, S, T]): ModifyImpl[ThisCan | ThatCan, S0, T0, A, B] = 
    ModifyImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  override def preComposeReverseGet[ThatCan <: ReverseGet, S0, T0](impl1: ReverseGetImpl[ThatCan, S0, T0, S, T]): ModifyImpl[ThisCan | ThatCan, S0, T0, A, B] = 
    ModifyImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  override def andThen[ThatCan <: OpticCan, C, D](impl2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[ThisCan | ThatCan, S, T, C, D] = 
    impl2.preComposeModify(this)

  override def modify(f: A => B)(using ThisCan <:< Modify): S => T = _modify(f)

end ModifyImpl