package monocle.impl

import monocle.*
import monocle.internal.*

import scala.collection.mutable.ListBuffer

private[monocle] trait TraversalImpl[+Can <: GetMany & Modify, -Structure, +Modified, +Out, -In]
    extends FoldImpl[Can, Structure, Modified, Out, In]
    with SetterImpl[Can, Structure, Modified, Out, In]:
  optic1 =>

  protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[In])(s: Structure): F[Modified]

  override protected[impl] def toIterator(s: Structure): Iterator[Out] = {
    val buffer = ListBuffer.empty[Out]
    modifyA { value =>
      buffer += value; Proxy.nothing
    }(s)
    buffer.iterator
  }

  override protected[impl] def modify(f: Out => In): Structure => Modified =
    modifyA[Id](f)

  override protected[impl] def replace(in: In): Structure => Modified =
    modify(_ => in)

  protected def composeTraversal[Can2 >: Can <: GetMany & Modify, Out2, In2](
    optic2: TraversalImpl[Can2, Out, In, Out2, In2]
  ): TraversalImpl[Can2, Structure, Modified, Out2, In2] =
    new TraversalImpl:
      override def modifyA[F[+_]: Applicative](f: Out2 => F[In2])(s: Structure): F[Modified] =
        optic1.modifyA(out => optic2.modifyA(f)(out))(s)

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case traversal: TraversalImpl[Can2 & GetMany & Modify, Out, In, Out2, In2] => composeTraversal(traversal)
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, Out, In, Out2, In2]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "TraversalImpl"

end TraversalImpl
