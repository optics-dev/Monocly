package monocle.impl

import monocle.*
import monocle.internal.*

import scala.collection.mutable.ListBuffer

private[monocle] trait TraversalImpl[+Can <: GetMany & Modify, -Structure, +Modified, +Out, -B]
    extends FoldImpl[Can, Structure, Modified, Out, B]
    with SetterImpl[Can, Structure, Modified, Out, B]:
  optic1 =>

  protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[B])(s: Structure): F[Modified]

  override protected[impl] def toIterator(s: Structure): Iterator[Out] = {
    val buffer = ListBuffer.empty[Out]
    modifyA { value =>
      buffer += value; Proxy.nothing
    }(s)
    buffer.iterator
  }

  override protected[impl] def modify(f: Out => B): Structure => Modified =
    modifyA[Id](f)

  override protected[impl] def replace(b: B): Structure => Modified =
    modify(_ => b)

  protected def composeTraversal[Can2 >: Can <: GetMany & Modify, Out2, In2](
    optic2: TraversalImpl[Can2, Out, B, Out2, In2]
  ): TraversalImpl[Can2, Structure, Modified, Out2, In2] =
    new TraversalImpl:
      override def modifyA[F[+_]: Applicative](f: Out2 => F[In2])(s: Structure): F[Modified] =
        optic1.modifyA(a => optic2.modifyA(f)(a))(s)

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, B, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case traversal: TraversalImpl[Can2 & GetMany & Modify, Out, B, Out2, In2] => composeTraversal(traversal)
      case fold: FoldImpl[Can2 & GetMany, Out, B, Out2, In2]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, Out, B, Out2, In2]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "TraversalImpl"

end TraversalImpl
