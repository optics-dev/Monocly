package monocle.impl

import monocle._

private[monocle] trait SetterImpl[+Can <: Modify, -S, +T, +A, -B] extends OpticImpl[Can, S, T, A, B]:
  optic1 =>

  protected[impl] def modify(f: A => B): S => T
  protected[impl] def replace(b: B): S => T

  protected def composeSetter[Can2 >: Can <: Modify, C, D](
    optic2: SetterImpl[Can2, A, B, C, D]
  ): SetterImpl[Can2, S, T, C, D] =
    new SetterImpl:
      override def modify(f: C => D): S => T =
        optic1.modify(optic2.modify(f))

      override def replace(d: D): S => T =
        optic1.modify(optic2.replace(d))

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case setter: SetterImpl[Can2 & Modify, A, B, C, D] => composeSetter(setter)
      case _                                                => NullOpticImpl

  override def toString: String =
    "SetterImpl"

end SetterImpl
