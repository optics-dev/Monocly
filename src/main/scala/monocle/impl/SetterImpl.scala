package monocle.impl

import monocle._

trait SetterImpl[+ThisCan <: Modify, -S, +T, +A, -B] extends OpticImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def modify(f: A => B): S => T
  protected[impl] def replace(b: B): S => T

  protected def composeSetter[ThatCan >: ThisCan <: Modify, C, D](optic2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[ThatCan, S, T, C, D] = 
    new SetterImpl:
      override def modify(f: C => D): S => T = 
        optic1.modify(optic2.modify(f))

      override def replace(d: D): S => T = 
        optic1.modify(optic2.replace(d))

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
      case _ => NullOpticImpl

  override def toString: String = 
    "SetterImpl"

end SetterImpl