package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait GetterImpl[+ThisCan <: Get, -S, +T, +A, -B]
    extends OptionalGetterImpl[ThisCan, S, T, A, B]
    with NonEmptyFoldImpl[ThisCan, S, T, A, B]:
  optic1 =>

  protected[impl] def get(s: S): A

  override protected[impl] def getOption(s: S): Option[A] =
    Some(get(s))

  override protected[impl] def toIterator(s: S): Iterator[A] =
    Iterator.single(get(s))

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M =
    f(get(s))

  protected def composeGetter[ThatCan >: ThisCan <: Get, C, D](
    optic2: GetterImpl[ThatCan, A, B, C, D]
  ): GetterImpl[ThatCan, S, T, C, D] =
    new GetterImpl:
      override def get(s: S): C =
        optic2.get(optic1.get(s))

  override def andThen[ThatCan >: ThisCan, C, D](
    optic2: OpticImpl[ThatCan, A, B, C, D]
  ): OpticImpl[ThatCan, S, T, C, D] =
    optic2 match
      case getter: GetterImpl[ThatCan & Get, A, B, C, D]                => composeGetter(getter)
      case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D]  => composeOptionalGetter(getOpt)
      case neFold: NonEmptyFoldImpl[ThatCan & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D]                => composeFold(fold)
      case _                                                            => NullOpticImpl

  override def toString: String =
    "GetterImpl"

end GetterImpl
