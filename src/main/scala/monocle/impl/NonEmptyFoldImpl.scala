package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait NonEmptyFoldImpl[+Can <: GetOneOrMore, -Structure, +Modified, +Out, -In] extends FoldImpl[Can, Structure, Modified, Out, In]:
  optic1 =>

  protected[impl] def nonEmptyFoldMap[M: Semigroup](f: Out => M)(s: Structure): M

  protected def composeNonEmptyFold[Can2 >: Can <: GetOneOrMore, Out2, In2](
    optic2: NonEmptyFoldImpl[Can2, Out, In, Out2, In2]
  ): NonEmptyFoldImpl[Can2, Structure, Modified, Out2, In2] =
    new NonEmptyFoldImpl:
      override def nonEmptyFoldMap[M: Semigroup](f: Out2 => M)(s: Structure): M =
        optic1.nonEmptyFoldMap(out => optic2.nonEmptyFoldMap(f)(out))(s)
      override def toIterator(s: Structure): Iterator[Out2] = optic1.toIterator(s).flatMap(optic2.toIterator)

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, Out, In, Out2, In2] => composeNonEmptyFold(neFold)
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2]                => composeFold(fold)
      case _                                                            => NullOpticImpl

  override def toString: String =
    "NonEmptyFoldImpl"

end NonEmptyFoldImpl
