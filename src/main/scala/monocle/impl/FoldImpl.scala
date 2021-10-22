package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait FoldImpl[+Can <: GetMany, -Structure, +Modified, +Out, -In] extends OpticImpl[Can, Structure, Modified, Out, In]:
  optic1 =>

  protected[impl] def toIterator(s: Structure): Iterator[Out]

  protected def composeFold[Can2 >: Can <: GetMany, Out2, In2](
    optic2: FoldImpl[Can2, Out, In, Out2, In2]
  ): FoldImpl[Can2, Structure, Modified, Out2, In2] =
    new FoldImpl:
      override protected[impl] def toIterator(s: Structure): Iterator[Out2] =
        optic1.toIterator(s).flatMap(optic2.toIterator)

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2] => composeFold(fold)
      case _                                             => NullOpticImpl

  override def toString: String =
    "FoldImpl"

end FoldImpl
