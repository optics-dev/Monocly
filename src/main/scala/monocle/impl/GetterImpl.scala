package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait GetterImpl[+Can <: Get, -Structure, +Modified, +Out, -In]
    extends OptionalGetterImpl[Can, Structure, Modified, Out, In]
    with NonEmptyFoldImpl[Can, Structure, Modified, Out, In]:
  optic1 =>

  protected[impl] def get(s: Structure): Out

  override protected[impl] def getOption(s: Structure): Option[Out] =
    Some(get(s))

  override protected[impl] def toIterator(s: Structure): Iterator[Out] =
    Iterator.single(get(s))

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: Out => M)(s: Structure): M =
    f(get(s))

  protected def composeGetter[Can2 >: Can <: Get, Out2, In2](
    optic2: GetterImpl[Can2, Out, In, Out2, In2]
  ): GetterImpl[Can2, Structure, Modified, Out2, In2] =
    new GetterImpl:
      override def get(s: Structure): Out2 =
        optic2.get(optic1.get(s))

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case getter: GetterImpl[Can2 & Get, Out, In, Out2, In2]                => composeGetter(getter)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, Out, In, Out2, In2]  => composeOptionalGetter(getOpt)
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, Out, In, Out2, In2] => composeNonEmptyFold(neFold)
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2]                => composeFold(fold)
      case _                                                            => NullOpticImpl

  override def toString: String =
    "GetterImpl"

end GetterImpl
