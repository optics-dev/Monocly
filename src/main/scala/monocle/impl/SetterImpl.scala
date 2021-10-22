package monocle.impl

import monocle._

private[monocle] trait SetterImpl[+Can <: Modify, -Structure, +Modified, +Out, -B] extends OpticImpl[Can, Structure, Modified, Out, B]:
  optic1 =>

  protected[impl] def modify(f: Out => B): Structure => Modified
  protected[impl] def replace(b: B): Structure => Modified

  protected def composeSetter[Can2 >: Can <: Modify, Out2, In2](
    optic2: SetterImpl[Can2, Out, B, Out2, In2]
  ): SetterImpl[Can2, Structure, Modified, Out2, In2] =
    new SetterImpl:
      override def modify(f: Out2 => In2): Structure => Modified =
        optic1.modify(optic2.modify(f))

      override def replace(d: In2): Structure => Modified =
        optic1.modify(optic2.replace(d))

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, B, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case setter: SetterImpl[Can2 & Modify, Out, B, Out2, In2] => composeSetter(setter)
      case _                                                => NullOpticImpl

  override def toString: String =
    "SetterImpl"

end SetterImpl
