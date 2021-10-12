package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait OptionalGetterImpl[+ThisCan <: GetOption, -S, +T, +A, -B] extends FoldImpl[ThisCan, S, T, A, B]:
  optic1 =>

  protected[impl] def getOption(s: S): Option[A]

  override protected[impl] def toIterator(s: S): Iterator[A] =
    getOption(s).iterator

  protected def composeOptionalGetter[ThatCan >: ThisCan <: GetOption, C, D](
    optic2: OptionalGetterImpl[ThatCan, A, B, C, D]
  ): OptionalGetterImpl[ThatCan, S, T, C, D] =
    new OptionalGetterImpl:
      override def getOption(s: S): Option[C] =
        optic1.getOption(s).flatMap(optic2.getOption)

  override def andThen[ThatCan >: ThisCan, C, D](
    optic2: OpticImpl[ThatCan, A, B, C, D]
  ): OpticImpl[ThatCan, S, T, C, D] =
    optic2 match
      case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D]               => composeFold(fold)
      case _                                                           => NullOpticImpl

  override def toString: String =
    "OptionalGetterImpl"

end OptionalGetterImpl
