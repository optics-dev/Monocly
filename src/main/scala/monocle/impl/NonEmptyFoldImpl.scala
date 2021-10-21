package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait NonEmptyFoldImpl[+Can <: GetOneOrMore, -S, +T, +A, -B] extends FoldImpl[Can, S, T, A, B]:
  optic1 =>

  protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M

  protected def composeNonEmptyFold[Can2 >: Can <: GetOneOrMore, C, D](
    optic2: NonEmptyFoldImpl[Can2, A, B, C, D]
  ): NonEmptyFoldImpl[Can2, S, T, C, D] =
    new NonEmptyFoldImpl:
      override def nonEmptyFoldMap[M: Semigroup](f: C => M)(s: S): M =
        optic1.nonEmptyFoldMap(a => optic2.nonEmptyFoldMap(f)(a))(s)
      override def toIterator(s: S): Iterator[C] = optic1.toIterator(s).flatMap(optic2.toIterator)

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D]                => composeFold(fold)
      case _                                                            => NullOpticImpl

  override def toString: String =
    "NonEmptyFoldImpl"

end NonEmptyFoldImpl
