package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait PrismImpl[+Can <: GetOption & ReverseGet, -Structure, +Modified, +Out, -B]
    extends OptionalImpl[Can, Structure, Modified, Out, B]:
  optic1 =>

  protected[impl] def reverseGet(b: B): Modified

  override protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[B])(s: Structure): F[Modified] =
    getOrModify(s).fold(t => Applicative[F].pure(t), a => Applicative[F].map(f(a))(reverseGet))

  override protected[impl] def modify(f: Out => B): Structure => Modified = s => getOrModify(s).fold(identity, a => reverseGet(f(a)))

  override protected[impl] def replace(b: B): Structure => Modified =
    modify(_ => b)

  protected def composePrism[Can2 >: Can <: GetOption & ReverseGet, Out2, In2](
    optic2: PrismImpl[Can2, Out, B, Out2, In2]
  ): PrismImpl[Can2, Structure, Modified, Out2, In2] =
    new PrismImpl:
      override def getOrModify(s: Structure): Either[Modified, Out2] =
        optic1.getOrModify(s).flatMap(a => optic2.getOrModify(a).left.map(optic1.replace(_)(s)))

      override def reverseGet(d: In2): Modified =
        optic1.reverseGet(optic2.reverseGet(d))

      override def getOption(s: Structure): Option[Out2] =
        optic1.getOption(s) flatMap optic2.getOption

  end composePrism

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, B, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case prism: PrismImpl[Can2 & GetOption & ReverseGet, Out, B, Out2, In2]   => composePrism(prism)
      case optional: OptionalImpl[Can2 & GetOption & Modify, Out, B, Out2, In2] => composeOptional(optional)
      case traversal: TraversalImpl[Can2 & GetMany & Modify, Out, B, Out2, In2] => composeTraversal(traversal)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, Out, B, Out2, In2]      => composeOptionalGetter(getOpt)
      case fold: FoldImpl[Can2 & GetMany, Out, B, Out2, In2]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, Out, B, Out2, In2]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "PrismImpl"

end PrismImpl
