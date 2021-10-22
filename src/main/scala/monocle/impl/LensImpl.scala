package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait LensImpl[+Can <: Get & Modify, -Structure, +Modified, +Out, -B]
    extends OptionalImpl[Can, Structure, Modified, Out, B]
    with NonEmptyTraversalImpl[Can, Structure, Modified, Out, B]
    with GetterImpl[Can, Structure, Modified, Out, B]:

  optic1 =>

  protected[impl] def get(s: Structure): Out
  protected[impl] def replace(b: B): Structure => Modified
  protected[impl] def modifyF[F[+_]: Functor](f: Out => F[B])(s: Structure): F[Modified]
  protected[impl] def modify(f: Out => B): Structure => Modified

  override protected[impl] def getOrModify(s: Structure): Either[Modified, Out] =
    Right(get(s))

  override protected[impl] def getOption(s: Structure): Option[Out] =
    Some(get(s))

  override protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[B])(s: Structure): F[Modified] =
    modifyF(f)(s)

  override protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: Out => F[B])(s: Structure): F[Modified] =
    modifyF(f)(s)

  override protected[impl] def toIterator(s: Structure): Iterator[Out] =
    Iterator.single(get(s))

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: Out => M)(s: Structure): M =
    f(get(s))

  protected def composeLens[Can2 >: Can <: Get & Modify, Out2, In2](
    optic2: LensImpl[Can2, Out, B, Out2, In2]
  ): LensImpl[Can2, Structure, Modified, Out2, In2] =
    new LensImpl:
      override def get(s: Structure): Out2 =
        optic2.get(optic1.get(s))

      override def replace(d: In2): Structure => Modified =
        optic1.modify(optic2.replace(d))

      override def modifyF[F[+_]: Functor](f: Out2 => F[In2])(s: Structure): F[Modified] =
        optic1.modifyF(optic2.modifyF(f))(s)

      override def modify(f: Out2 => In2): Structure => Modified =
        optic1.modify(optic2.modify(f))

  end composeLens

  override def andThen[Can2 >: Can, Out2, In2](
    optic2: OpticImpl[Can2, Out, B, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case lens: LensImpl[Can2 & Get & Modify, Out, B, Out2, In2]               => composeLens(lens)
      case optional: OptionalImpl[Can2 & GetOption & Modify, Out, B, Out2, In2] => composeOptional(optional)
      case neTraversal: NonEmptyTraversalImpl[Can2 & GetOneOrMore & Modify, Out, B, Out2, In2] =>
        composeNonEmptyTraversal(neTraversal)
      case getter: GetterImpl[Can2 & Get, Out, B, Out2, In2]                    => composeGetter(getter)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, Out, B, Out2, In2]      => composeOptionalGetter(getOpt)
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, Out, B, Out2, In2]     => composeNonEmptyFold(neFold)
      case traversal: TraversalImpl[Can2 & GetMany & Modify, Out, B, Out2, In2] => composeTraversal(traversal)
      case fold: FoldImpl[Can2 & GetMany, Out, B, Out2, In2]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, Out, B, Out2, In2]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "LensImpl"

end LensImpl
