package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait PrismImpl[+Can <: GetOption & ReverseGet, -Structure, +Modified, +Out, -In]
    extends OptionalImpl[Can, Structure, Modified, Out, In]:
  optic1 =>

  protected[impl] def reverseGet(in: In): Modified

  override protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[In])(s: Structure): F[Modified] =
    getOrModify(s).fold(m => Applicative[F].pure(m), out => Applicative[F].map(f(out))(reverseGet))

  override protected[impl] def modify(f: Out => In): Structure => Modified = s => getOrModify(s).fold(identity, out => reverseGet(f(out)))

  override protected[impl] def replace(in: In): Structure => Modified =
    modify(_ => in)

  protected def composePrism[Can2 >: Can <: GetOption & ReverseGet, Out2, In2](
    optic2: PrismImpl[Can2, Out, In, Out2, In2]
  ): PrismImpl[Can2, Structure, Modified, Out2, In2] =
    new PrismImpl:
      override def getOrModify(s: Structure): Either[Modified, Out2] =
        optic1.getOrModify(s).flatMap(out => optic2.getOrModify(out).left.map(optic1.replace(_)(s)))

      override def reverseGet(d: In2): Modified =
        optic1.reverseGet(optic2.reverseGet(d))

      override def getOption(s: Structure): Option[Out2] =
        optic1.getOption(s) flatMap optic2.getOption

  end composePrism

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case prism: PrismImpl[Can2 & GetOption & ReverseGet, Out, In, Out2, In2]   => composePrism(prism)
      case optional: OptionalImpl[Can2 & GetOption & Modify, Out, In, Out2, In2] => composeOptional(optional)
      case traversal: TraversalImpl[Can2 & GetMany & Modify, Out, In, Out2, In2] => composeTraversal(traversal)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, Out, In, Out2, In2]      => composeOptionalGetter(getOpt)
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, Out, In, Out2, In2]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "PrismImpl"

end PrismImpl
