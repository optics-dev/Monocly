package optics.poly


class ReverseGetImpl[ThisCan <: ReverseGet, -S, +T, +A, -B](val modify: (A => B) => S => T, val reverseGet: B => T) extends SetterImpl[ThisCan, S, T, A, B]:

  override def preComposeModify[ThatCan <: Modify, S0, T0](impl1: ModifyImpl[ThatCan, S0, T0, S, T]): ModifyImpl[ThisCan | ThatCan, S0, T0, A, B] = 
    ModifyImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  override def preComposeReverseGet[ThatCan <: ReverseGet, S0, T0](impl1: ReverseGetImpl[ThatCan, S0, T0, S, T]): ReverseGetImpl[ThisCan | ThatCan, S0, T0, A, B] = 
    ReverseGetImpl(
      f => s0 => impl1.modify(s => modify(f)(s))(s0), 
      b => impl1.reverseGet(reverseGet(b)))

  override def andThen[ThatCan <: OpticCan, C, D](impl2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[ThisCan | ThatCan, S, T, C, D] = 
    impl2.preComposeReverseGet(this)

  override def doModify(f: A => B)(using ThisCan <:< Modify): S => T = modify(f)
  override def doReverseGet(using ThisCan <:< ReverseGet): B => T = reverseGet

end ReverseGetImpl