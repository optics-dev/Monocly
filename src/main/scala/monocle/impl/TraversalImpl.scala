package monocle.impl

import monocle.*
import monocle.internal.*

import scala.collection.mutable.ListBuffer

private[monocle] trait TraversalImpl[+Can <: GetMany & Modify, -S, +T, +A, -B]
    extends FoldImpl[Can, S, T, A, B]
    with SetterImpl[Can, S, T, A, B]:
  optic1 =>

  protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T]

  override protected[impl] def toIterator(s: S): Iterator[A] = {
    val buffer = ListBuffer.empty[A]
    modifyA { value =>
      buffer += value; Proxy.nothing
    }(s)
    buffer.iterator
  }

  override protected[impl] def modify(f: A => B): S => T =
    modifyA[Id](f)

  override protected[impl] def replace(b: B): S => T =
    modify(_ => b)

  protected def composeTraversal[Can2 >: Can <: GetMany & Modify, C, D](
    optic2: TraversalImpl[Can2, A, B, C, D]
  ): TraversalImpl[Can2, S, T, C, D] =
    new TraversalImpl:
      override def modifyA[F[+_]: Applicative](f: C => F[D])(s: S): F[T] =
        optic1.modifyA(a => optic2.modifyA(f)(a))(s)

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case traversal: TraversalImpl[Can2 & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, A, B, C, D]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "TraversalImpl"

end TraversalImpl
