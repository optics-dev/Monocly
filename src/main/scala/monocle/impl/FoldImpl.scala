package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait FoldImpl[+ThisCan <: GetMany, -S, +T, +A, -B] extends OpticImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M

  protected def composeFold[ThatCan >: ThisCan <: GetMany, C, D](optic2: FoldImpl[ThatCan, A, B, C, D]): FoldImpl[ThatCan, S, T, C, D] = 
    new FoldImpl:
      override def foldMap[M: Monoid](f: C => M)(s: S): M = 
        optic1.foldMap(a => optic2.foldMap(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
      case _ => NullOpticImpl

  override def toString: String = 
    "FoldImpl"

end FoldImpl