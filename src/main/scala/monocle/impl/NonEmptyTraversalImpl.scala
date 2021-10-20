package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait NonEmptyTraversalImpl[+Can <: EditOneOrMore, -S, +T, +A, -B]
    extends NonEmptyFoldImpl[Can, S, T, A, B]
    with TraversalImpl[Can, S, T, A, B]:

  optic1 =>

  protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T]

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M =
    nonEmptyModifyA[[x] =>> Const[M, x]](a => Const(f(a)))(s).getConst

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    nonEmptyModifyA(f)(s)

  protected def composeNonEmptyTraversal[Can2 >: Can <: EditOneOrMore, C, D](
    optic2: NonEmptyTraversalImpl[Can2, A, B, C, D]
  ): NonEmptyTraversalImpl[Can2, S, T, C, D] =
    new NonEmptyTraversalImpl:
      override def nonEmptyModifyA[F[+_]: Apply](f: C => F[D])(s: S): F[T] =
        optic1.nonEmptyModifyA(a => optic2.nonEmptyModifyA(f)(a))(s)

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case neTraversal: NonEmptyTraversalImpl[Can2 & EditOneOrMore, A, B, C, D] =>
        composeNonEmptyTraversal(neTraversal)
      case traversal: TraversalImpl[Can2 & EditMany, A, B, C, D] => composeTraversal(traversal)
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, A, B, C, D]     => composeNonEmptyFold(neFold)
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, A, B, C, D]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "NonEmptyTraversalImpl"

end NonEmptyTraversalImpl
