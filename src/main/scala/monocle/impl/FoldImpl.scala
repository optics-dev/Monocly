package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait FoldImpl[+Can <: GetMany, -S, +T, +A, -B] extends OpticImpl[Can, S, T, A, B]:
  optic1 =>

  protected[impl] def toIterator(s: S): Iterator[A]

  protected def composeFold[Can2 >: Can <: GetMany, C, D](
    optic2: FoldImpl[Can2, A, B, C, D]
  ): FoldImpl[Can2, S, T, C, D] =
    new FoldImpl:
      override protected[impl] def toIterator(s: S): Iterator[C] =
        optic1.toIterator(s).flatMap(optic2.toIterator)

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D] => composeFold(fold)
      case _                                             => NullOpticImpl

  override def toString: String =
    "FoldImpl"

end FoldImpl
