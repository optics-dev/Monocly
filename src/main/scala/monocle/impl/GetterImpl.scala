package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait GetterImpl[+Can <: Get, -S, +T, +A, -B]
    extends OptionalGetterImpl[Can, S, T, A, B]
    with NonEmptyFoldImpl[Can, S, T, A, B]:
  optic1 =>

  protected[impl] def get(s: S): A

  override protected[impl] def getOption(s: S): Option[A] =
    Some(get(s))

  override protected[impl] def toIterator(s: S): Iterator[A] =
    Iterator.single(get(s))

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M =
    f(get(s))

  protected def composeGetter[Can2 >: Can <: Get, C, D](
    optic2: GetterImpl[Can2, A, B, C, D]
  ): GetterImpl[Can2, S, T, C, D] =
    new GetterImpl:
      override def get(s: S): C =
        optic2.get(optic1.get(s))

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case getter: GetterImpl[Can2 & Get, A, B, C, D]                => composeGetter(getter)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, A, B, C, D]  => composeOptionalGetter(getOpt)
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D]                => composeFold(fold)
      case _                                                            => NullOpticImpl

  override def toString: String =
    "GetterImpl"

end GetterImpl
