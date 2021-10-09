package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait NonEmptyTraversalImpl[+ThisCan <: GetOneOrMore & Modify, -S, +T, +A, -B] 
    extends NonEmptyFoldImpl[ThisCan, S, T, A, B] 
    with TraversalImpl[ThisCan, S, T, A, B]:

  optic1 => 

  protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T]

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = 
    nonEmptyModifyA[[x] =>> Const[M, x]](a => Const(f(a)))(s).getConst

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = 
    nonEmptyModifyA(f)(s)

  protected def composeNonEmptyTraversal[ThatCan >: ThisCan <: GetOneOrMore & Modify, C, D](optic2: NonEmptyTraversalImpl[ThatCan, A, B, C, D]): NonEmptyTraversalImpl[ThatCan, S, T, C, D] = 
    new NonEmptyTraversalImpl:
      override def nonEmptyModifyA[F[+_]: Apply](f: C => F[D])(s: S): F[T] =
        optic1.nonEmptyModifyA(a => optic2.nonEmptyModifyA(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case neTraversal: NonEmptyTraversalImpl[ThatCan & GetOneOrMore & Modify, A, B, C, D] => composeNonEmptyTraversal(neTraversal)
      case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case neFold: NonEmptyFoldImpl[ThatCan & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
      case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
      case _ => NullOpticImpl

  override def toString: String = 
    "NonEmptyTraversalImpl"

end NonEmptyTraversalImpl