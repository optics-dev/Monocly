package optics.poly

trait SetterImpl[+ThisCan <: OpticCan, -S, +T, +A, -B]:

  def preComposeModify[S0, T0](impl1: ModifyImpl[S0, T0, S, T]): SetterImpl[ThisCan | Modify, S0, T0, A, B]
  def preComposeReverseGet[S0, T0](impl1: ReverseGetImpl[S0, T0, S, T]): SetterImpl[ThisCan | ReverseGet, S0, T0, A, B]

  def andThen[ThatCan <: OpticCan, C, D](impl2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[ThisCan | ThatCan, S, T, C, D]

  def doModify(f: A => B)(using ThisCan <:< Modify): S => T = sys.error("This optic does not support 'modify'")
  def doReverseGet(using ThisCan <:< ReverseGet): B => T = sys.error("This optic does not support 'replaceAll'")

end SetterImpl