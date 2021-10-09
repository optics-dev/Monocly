package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait PrismImpl[+ThisCan <: GetOption & ReverseGet, -S, +T, +A, -B] extends OptionalImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def reverseGet(b: B): T

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    getOrModify(s).fold(
      t => Applicative[F].pure(t), 
      a => Applicative[F].map(f(a))(reverseGet))

  override protected[impl] def modify(f: A => B): S => T = s => 
    getOrModify(s).fold(identity, a => reverseGet(f(a)))

  override protected[impl] def replace(b: B): S => T = 
    modify(_ => b)

  protected def composePrism[ThatCan >: ThisCan <: GetOption & ReverseGet, C, D](optic2: PrismImpl[ThatCan, A, B, C, D]): PrismImpl[ThatCan, S, T, C, D] = 
    new PrismImpl:
      override def getOrModify(s: S): Either[T, C] = 
        optic1.getOrModify(s).flatMap(a => optic2.getOrModify(a).left.map(optic1.replace(_)(s)))

      override def reverseGet(d: D): T = 
        optic1.reverseGet(optic2.reverseGet(d))

      override def getOption(s: S): Option[C] = 
        optic1.getOption(s) flatMap optic2.getOption

  end composePrism

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case prism: PrismImpl[ThatCan & GetOption & ReverseGet, A, B, C, D] => composePrism(prism)
      case optional: OptionalImpl[ThatCan & GetOption & Modify, A, B, C, D] => composeOptional(optional)
      case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
      case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
      case _ => NullOpticImpl

  override def toString: String = 
    "PrismImpl"

end PrismImpl