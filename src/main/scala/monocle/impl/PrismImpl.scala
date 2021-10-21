package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait PrismImpl[+Can <: GetOption & ReverseGet, -S, +T, +A, -B]
    extends OptionalImpl[Can, S, T, A, B]:
  optic1 =>

  protected[impl] def reverseGet(b: B): T

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    getOrModify(s).fold(t => Applicative[F].pure(t), a => Applicative[F].map(f(a))(reverseGet))

  override protected[impl] def modify(f: A => B): S => T = s => getOrModify(s).fold(identity, a => reverseGet(f(a)))

  override protected[impl] def replace(b: B): S => T =
    modify(_ => b)

  protected def composePrism[Can2 >: Can <: GetOption & ReverseGet, C, D](
    optic2: PrismImpl[Can2, A, B, C, D]
  ): PrismImpl[Can2, S, T, C, D] =
    new PrismImpl:
      override def getOrModify(s: S): Either[T, C] =
        optic1.getOrModify(s).flatMap(a => optic2.getOrModify(a).left.map(optic1.replace(_)(s)))

      override def reverseGet(d: D): T =
        optic1.reverseGet(optic2.reverseGet(d))

      override def getOption(s: S): Option[C] =
        optic1.getOption(s) flatMap optic2.getOption

  end composePrism

  override def andThen[Can2 >: Can, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case prism: PrismImpl[Can2 & GetOption & ReverseGet, A, B, C, D]   => composePrism(prism)
      case optional: OptionalImpl[Can2 & GetOption & Modify, A, B, C, D] => composeOptional(optional)
      case traversal: TraversalImpl[Can2 & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, A, B, C, D]      => composeOptionalGetter(getOpt)
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, A, B, C, D]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "PrismImpl"

end PrismImpl
