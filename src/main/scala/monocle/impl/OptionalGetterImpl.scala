package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait OptionalGetterImpl[+Can <: GetOption, -S, +T, +A, -B] extends FoldImpl[Can, S, T, A, B]:
  optic1 =>

  protected[impl] def getOption(s: S): Option[A]

  override protected[impl] def toIterator(s: S): Iterator[A] =
    getOption(s).iterator

  protected def composeOptionalGetter[Can2 >: Can <: GetOption, C, D](
    optic2: OptionalGetterImpl[Can2, A, B, C, D]
  ): OptionalGetterImpl[Can2, S, T, C, D] =
    new OptionalGetterImpl:
      override def getOption(s: S): Option[C] =
        optic1.getOption(s).flatMap(optic2.getOption)

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case getOpt: OptionalGetterImpl[Can2 & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D]               => composeFold(fold)
      case _                                                           => NullOpticImpl

  override def toString: String =
    "OptionalGetterImpl"

end OptionalGetterImpl
