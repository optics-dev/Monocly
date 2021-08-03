package optics.poly

sealed trait SetImpl[+ThisCan <: OpticCan, -S, +T, +A, -B]:

  def preComposeReplace[S0, T0](impl1: ReplaceImpl[S0, T0, S, T]): SetImpl[ThisCan | Replace, S0, T0, A, B]
  def preComposeReplaceAll[S0, T0](impl1: ReplaceAllImpl[S0, T0, S, T]): SetImpl[ThisCan | ReplaceAll, S0, T0, A, B]

  def andThen[ThatCan <: OpticCan, C, D](impl2: SetImpl[ThatCan, A, B, C, D]): SetImpl[ThisCan | ThatCan, S, T, C, D]

  def doModify(f: A => B)(using ThisCan <:< Replace): S => T = sys.error("This optic does not support 'modify'")
  def doReplace(b: B)(using ThisCan <:< Replace): S => T = sys.error("This optic does not support 'replace'")
  def doReplaceAll(b: B)(using ThisCan <:< ReplaceAll): T = sys.error("This optic does not support 'replaceAll'")

end SetImpl


object ReplaceNoneImpl extends SetImpl[Nothing, Any, Nothing, Nothing, Any]:
  override def preComposeReplace[S0, T0](impl1: ReplaceImpl[S0, T0, Any, Nothing]) = ReplaceNoneImpl
  override def preComposeReplaceAll[S0, T0](impl1: ReplaceAllImpl[S0, T0, Any, Nothing]) = ReplaceNoneImpl
  override def andThen[ThatCan <: OpticCan, C, D](impl2: SetImpl[ThatCan, Nothing, Any, C, D]) = ReplaceNoneImpl
end ReplaceNoneImpl


class ReplaceImpl[-S, +T, +A, -B](val modify: (A => B) => S => T) extends SetImpl[Replace, S, T, A, B]:

  // def replace(b: B): S => Ts

  def preComposeReplace[S0, T0](impl1: ReplaceImpl[S0, T0, S, T]): ReplaceImpl[S0, T0, A, B] = 
    ReplaceImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  def preComposeReplaceAll[S0, T0](impl1: ReplaceAllImpl[S0, T0, S, T]): ReplaceImpl[S0, T0, A, B] = ???
    //ReplaceImpl(f => s0 => impl1.modify(s => modify(f)(s))(s0))

  def andThen[ThatCan <: OpticCan, C, D](impl2: SetImpl[ThatCan, A, B, C, D]): SetImpl[Replace | ThatCan, S, T, C, D] = 
    impl2.preComposeReplace(this)

  override def doModify(f: A => B)(using Replace <:< Replace): S => T = modify(f)
  override def doReplace(b: B)(using Replace <:< Replace): S => T = modify(_ => b)

end ReplaceImpl


class ReplaceAllImpl[-S, +T, +A, -B](val replaceAll: B => T) extends SetImpl[ReplaceAll, S, T, A, B]:

  final def replace(b: B): S => T = 
    _ => replaceAll(b)

  final def modify(f: A => B): S => T = ???
    //s => 

  override def preComposeReplace[S0, T0](impl1: ReplaceImpl[S0, T0, S, T]): ReplaceImpl[S0, T0, A, B] = ???
  override def preComposeReplaceAll[S0, T0](impl1: ReplaceAllImpl[S0, T0, S, T]): ReplaceAllImpl[S0, T0, A, B] = ???

  def andThen[ThatCan <: OpticCan, C, D](impl2: SetImpl[ThatCan, A, B, C, D]): SetImpl[ReplaceAll | ThatCan, S, T, C, D] = 
    impl2.preComposeReplaceAll(this)

  override def doModify(f: A => B)(using ReplaceAll <:< Replace): S => T = sys.error("This optic does not support 'modify'")
  override def doReplace(b: B)(using ReplaceAll <:< Replace): S => T = sys.error("This optic does not support 'replace'")
  override def doReplaceAll(b: B)(using ReplaceAll <:< ReplaceAll): T = replaceAll(b)

end ReplaceAllImpl