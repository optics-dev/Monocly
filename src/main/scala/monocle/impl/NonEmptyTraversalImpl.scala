package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait NonEmptyTraversalImpl[+Can <: GetOneOrMore & Modify, -Structure, +Modified, +Out, -In]
    extends NonEmptyFoldImpl[Can, Structure, Modified, Out, In]
    with TraversalImpl[Can, Structure, Modified, Out, In]:

  optic1 =>

  protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: Out => F[In])(s: Structure): F[Modified]

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: Out => M)(s: Structure): M =
    nonEmptyModifyA[[x] =>> Const[M, x]](out => Const(f(out)))(s).getConst

  override protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[In])(s: Structure): F[Modified] =
    nonEmptyModifyA(f)(s)

  protected def composeNonEmptyTraversal[Can2 >: Can <: GetOneOrMore & Modify, Out2, In2](
    optic2: NonEmptyTraversalImpl[Can2, Out, In, Out2, In2]
  ): NonEmptyTraversalImpl[Can2, Structure, Modified, Out2, In2] =
    new NonEmptyTraversalImpl:
      override def nonEmptyModifyA[F[+_]: Apply](f: Out2 => F[In2])(s: Structure): F[Modified] =
        optic1.nonEmptyModifyA(out => optic2.nonEmptyModifyA(f)(out))(s)

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case neTraversal: NonEmptyTraversalImpl[Can2 & GetOneOrMore & Modify, Out, In, Out2, In2] =>
        composeNonEmptyTraversal(neTraversal)
      case traversal: TraversalImpl[Can2 & GetMany & Modify, Out, In, Out2, In2] => composeTraversal(traversal)
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, Out, In, Out2, In2]     => composeNonEmptyFold(neFold)
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, Out, In, Out2, In2]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "NonEmptyTraversalImpl"

end NonEmptyTraversalImpl
