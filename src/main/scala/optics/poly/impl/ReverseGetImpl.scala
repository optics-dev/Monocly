package optics.poly


class ReverseGetImpl[-S, +T, +A, -B](val modify: (A => B) => S => T, val reverseGet: B => T) extends SetterImpl[ReverseGet, S, T, A, B]:

  override def preComposeModify[S0, T0](impl1: ModifyImpl[S0, T0, S, T]): ModifyImpl[S0, T0, A, B] = 
    ModifyImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  override def preComposeReverseGet[S0, T0](impl1: ReverseGetImpl[S0, T0, S, T]): ReverseGetImpl[S0, T0, A, B] = 
    ReverseGetImpl(
      f => s0 => impl1.modify(s => modify(f)(s))(s0), 
      b => impl1.reverseGet(reverseGet(b)))

  override def andThen[ThatCan <: OpticCan, C, D](impl2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[ReverseGet | ThatCan, S, T, C, D] = 
    impl2.preComposeReverseGet(this)

  override def doModify(f: A => B)(using ReverseGet <:< Modify): S => T = modify(f)
  override def doReverseGet(using ReverseGet <:< ReverseGet): B => T = reverseGet

end ReverseGetImpl