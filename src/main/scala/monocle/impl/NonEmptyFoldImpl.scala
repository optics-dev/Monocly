package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait NonEmptyFoldImpl[+ThisCan <: GetOneOrMore, -S, +T, +A, -B] extends FoldImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M

  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = 
    nonEmptyFoldMap(f)(s)

  protected def composeNonEmptyFold[ThatCan >: ThisCan <: GetOneOrMore, C, D](optic2: NonEmptyFoldImpl[ThatCan, A, B, C, D]): NonEmptyFoldImpl[ThatCan, S, T, C, D] = 
    new NonEmptyFoldImpl:
      override def nonEmptyFoldMap[M: Semigroup](f: C => M)(s: S): M = optic1.nonEmptyFoldMap(a => optic2.nonEmptyFoldMap(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case neFold: NonEmptyFoldImpl[ThatCan & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
      case _ => NullOpticImpl

  override def toString: String = 
    "NonEmptyFoldImpl"

end NonEmptyFoldImpl