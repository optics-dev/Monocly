package monocle.impl

import monocle._
import monocle.internal._

trait OptionalImpl[+ThisCan <: GetOption & Modify, -S, +T, +A, -B] 
    extends TraversalImpl[ThisCan, S, T, A, B] 
    with OptionalGetterImpl[ThisCan, S, T, A, B]:

  optic1 => 

  protected[impl] def getOrModify(s: S): Either[T, A]
  protected[impl] def replace(b: B): S => T
  protected[impl] def getOption(s: S): Option[A]

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    getOrModify(s).fold(
      t => Applicative[F].pure(t),
      a => Applicative[F].map(f(a))(b => replace(b)(s)))

  override protected[impl] def modify(f: A => B): S => T = 
    s => getOrModify(s).fold(identity, a => replace(f(a))(s))

  protected def composeOptional[ThatCan >: ThisCan <: GetOption & Modify, C, D](optic2: OptionalImpl[ThatCan, A, B, C, D]): OptionalImpl[ThatCan, S, T, C, D] = 
    new OptionalImpl:
      override def getOrModify(s: S): Either[T, C] = 
        optic1
          .getOrModify(s)
          .flatMap(a => optic2.getOrModify(a).left.map(optic1.replace(_)(s)))

      override def replace(d: D): S => T = 
        optic1.modify(optic2.replace(d))

      override def getOption(s: S): Option[C] = 
        optic1.getOption(s) flatMap optic2.getOption

      override def modifyA[F[+_]: Applicative](f: C => F[D])(s: S): F[T] = 
        optic1.modifyA(optic2.modifyA(f))(s)

      override def modify(f: C => D): S => T = 
        optic1.modify(optic2.modify(f))

  end composeOptional

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case optional: OptionalImpl[ThatCan & GetOption & Modify, A, B, C, D] => composeOptional(optional)
      case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
      case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
      case _ => NullOpticImpl

  override def toString: String = 
    "OptionalImpl"

end OptionalImpl