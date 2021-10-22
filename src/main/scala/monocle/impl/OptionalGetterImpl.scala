package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait OptionalGetterImpl[+Can <: GetOption, -Structure, +Modified, +Out, -In] extends FoldImpl[Can, Structure, Modified, Out, In]:
  optic1 =>

  protected[impl] def getOption(s: Structure): Option[Out]

  override protected[impl] def toIterator(s: Structure): Iterator[Out] =
    getOption(s).iterator

  protected def composeOptionalGetter[Can2 >: Can <: GetOption, Out2, In2](
    optic2: OptionalGetterImpl[Can2, Out, In, Out2, In2]
  ): OptionalGetterImpl[Can2, Structure, Modified, Out2, In2] =
    new OptionalGetterImpl:
      override def getOption(s: Structure): Option[Out2] =
        optic1.getOption(s).flatMap(optic2.getOption)

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case getOpt: OptionalGetterImpl[Can2 & GetOption, Out, In, Out2, In2] => composeOptionalGetter(getOpt)
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2]               => composeFold(fold)
      case _                                                           => NullOpticImpl

  override def toString: String =
    "OptionalGetterImpl"

end OptionalGetterImpl
