package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait OptionalImpl[+Can <: GetOption & Modify, -Structure, +Modified, +Out, -In]
    extends TraversalImpl[Can, Structure, Modified, Out, In]
    with OptionalGetterImpl[Can, Structure, Modified, Out, In]:

  optic1 =>

  protected[impl] def getOrModify(s: Structure): Either[Modified, Out]
  protected[impl] def replace(in: In): Structure => Modified
  protected[impl] def getOption(s: Structure): Option[Out]

  override protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[In])(s: Structure): F[Modified] =
    getOrModify(s).fold(m => Applicative[F].pure(m), out => Applicative[F].map(f(out))(in => replace(in)(s)))

  override protected[impl] def modify(f: Out => In): Structure => Modified =
    s => getOrModify(s).fold(identity, out => replace(f(out))(s))

  protected def composeOptional[Can2 >: Can <: GetOption & Modify, Out2, In2](
    optic2: OptionalImpl[Can2, Out, In, Out2, In2]
  ): OptionalImpl[Can2, Structure, Modified, Out2, In2] =
    new OptionalImpl:
      override def getOrModify(s: Structure): Either[Modified, Out2] =
        optic1
          .getOrModify(s)
          .flatMap(out => optic2.getOrModify(out).left.map(optic1.replace(_)(s)))

      override def replace(d: In2): Structure => Modified =
        optic1.modify(optic2.replace(d))

      override def getOption(s: Structure): Option[Out2] =
        optic1.getOption(s) flatMap optic2.getOption

      override def modifyA[F[+_]: Applicative](f: Out2 => F[In2])(s: Structure): F[Modified] =
        optic1.modifyA(optic2.modifyA(f))(s)

      override def modify(f: Out2 => In2): Structure => Modified =
        optic1.modify(optic2.modify(f))

  end composeOptional

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, In, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case optional: OptionalImpl[Can2 & GetOption & Modify, Out, In, Out2, In2] => composeOptional(optional)
      case traversal: TraversalImpl[Can2 & GetMany & Modify, Out, In, Out2, In2] => composeTraversal(traversal)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, Out, In, Out2, In2]      => composeOptionalGetter(getOpt)
      case fold: FoldImpl[Can2 & GetMany, Out, In, Out2, In2]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, Out, In, Out2, In2]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "OptionalImpl"

end OptionalImpl
