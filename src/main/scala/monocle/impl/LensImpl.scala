package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait LensImpl[+ThisCan <: Get & Modify, -S, +T, +A, -B] 
    extends OptionalImpl[ThisCan, S, T, A, B] 
    with NonEmptyTraversalImpl[ThisCan, S, T, A, B] 
    with GetterImpl[ThisCan, S, T, A, B]:

  optic1 => 

  protected[impl] def get(s: S): A
  protected[impl] def replace(b: B): S => T
  protected[impl] def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T]
  protected[impl] def modify(f: A => B): S => T

  override protected[impl] def getOrModify(s: S): Either[T, A] = 
    Right(get(s))

  override protected[impl] def getOption(s: S): Option[A] = 
    Some(get(s))

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = 
    modifyF(f)(s)

  override protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] = 
    modifyF(f)(s)

  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = 
    f(get(s))

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = 
    f(get(s))

  protected def composeLens[ThatCan >: ThisCan <: Get & Modify, C, D](optic2: LensImpl[ThatCan, A, B, C, D]): LensImpl[ThatCan, S, T, C, D] = 
    new LensImpl:
      override def get(s: S): C = 
        optic2.get(optic1.get(s))

      override def replace(d: D): S => T = 
        optic1.modify(optic2.replace(d))

      override def modifyF[F[+_]: Functor](f: C => F[D])(s: S): F[T] = 
        optic1.modifyF(optic2.modifyF(f))(s)

      override def modify(f: C => D): S => T = 
        optic1.modify(optic2.modify(f))

  end composeLens

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case lens: LensImpl[ThatCan & Get & Modify, A, B, C, D] => composeLens(lens)
      case optional: OptionalImpl[ThatCan & GetOption & Modify, A, B, C, D] => composeOptional(optional)
      case neTraversal: NonEmptyTraversalImpl[ThatCan & GetOneOrMore & Modify, A, B, C, D] => composeNonEmptyTraversal(neTraversal)
      case getter: GetterImpl[ThatCan & Get, A, B, C, D] => composeGetter(getter)
      case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
      case neFold: NonEmptyFoldImpl[ThatCan & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
      case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
      case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
      case _ => NullOpticImpl

  override def toString: String = 
    "LensImpl"

end LensImpl