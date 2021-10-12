package monocle.impl

import monocle.*
import monocle.internal.*

import scala.collection.mutable.ListBuffer

private[monocle] trait TraversalImpl[+ThisCan <: GetMany & Modify, -S, +T, +A, -B] extends FoldImpl[ThisCan, S, T, A, B] with SetterImpl[ThisCan, S, T, A, B]:
  optic1 =>   

  protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T]

  override protected[impl] def toIterator(s: S): Iterator[A] = {
    val buffer = ListBuffer.empty[A]
    modifyA{ value => buffer += value ; Proxy.nothing }(s)
    buffer.iterator
  }


  override protected[impl] def modify(f: A => B): S => T =
    modifyA[Id](f)

  override protected[impl] def replace(b: B): S => T = 
    modify(_ => b)

  protected def composeTraversal[ThatCan >: ThisCan <: GetMany & Modify, C, D](optic2: TraversalImpl[ThatCan, A, B, C, D]): TraversalImpl[ThatCan, S, T, C, D] = 
    new TraversalImpl:
      override def modifyA[F[+_]: Applicative](f: C => F[D])(s: S): F[T] =
        optic1.modifyA(a => optic2.modifyA(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
      case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
      case _ => NullOpticImpl

  override def toString: String = 
    "TraversalImpl"

end TraversalImpl