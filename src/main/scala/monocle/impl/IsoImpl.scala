package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait IsoImpl[-S, +T, +A, -B]
    extends LensImpl[Get & ReverseGet, S, T, A, B]
    with PrismImpl[Get & ReverseGet, S, T, A, B]:

  optic1 =>

  protected[impl] def reverse: IsoImpl[B, A, T, S]

  override protected[impl] def toIterator(s: S): Iterator[A] =
    Iterator.single(get(s))

  override protected[impl] def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T] =
    Functor[F].map(f(get(s)))(reverseGet)

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    modifyF(f)(s)

  override protected[impl] def modify(f: A => B): S => T =
    s => reverseGet(f(get(s)))

  override protected[impl] def replace(b: B): S => T =
    _ => reverseGet(b)

  protected def composeIso[C, D](optic2: IsoImpl[A, B, C, D]): IsoImpl[S, T, C, D] =
    new IsoImpl:
      composeSelf =>
      override def get(s: S): C =
        optic2.get(optic1.get(s))

      override def reverseGet(d: D): T =
        optic1.reverseGet(optic2.reverseGet(d))

      override def reverse: IsoImpl[D, C, T, S] =
        new IsoImpl:
          override def get(d: D): T =
            optic1.reverseGet(optic2.reverseGet(d))

          override def reverseGet(s: S): C =
            optic2.get(optic1.get(s))

          override def reverse: IsoImpl[S, T, C, D] =
            composeSelf

      end reverse

  end composeIso

  override def andThen[Can2 >: Get & ReverseGet, C, D](
    optic2: OpticImpl[Can2, A, B, C, D]
  ): OpticImpl[Can2, S, T, C, D] =
    optic2 match
      case iso: IsoImpl[A, B, C, D]                                         => composeIso(iso)
      case lens: LensImpl[Can2 & Get & Modify, A, B, C, D]               => composeLens(lens)
      case prism: PrismImpl[Can2 & GetOption & ReverseGet, A, B, C, D]   => composePrism(prism)
      case optional: OptionalImpl[Can2 & GetOption & Modify, A, B, C, D] => composeOptional(optional)
      case neTraversal: NonEmptyTraversalImpl[Can2 & GetOneOrMore & Modify, A, B, C, D] =>
        composeNonEmptyTraversal(neTraversal)
      case getter: GetterImpl[Can2 & Get, A, B, C, D]                    => composeGetter(getter)
      case getOpt: OptionalGetterImpl[Can2 & GetOption, A, B, C, D]      => composeOptionalGetter(getOpt)
      case neFold: NonEmptyFoldImpl[Can2 & GetOneOrMore, A, B, C, D]     => composeNonEmptyFold(neFold)
      case traversal: TraversalImpl[Can2 & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
      case fold: FoldImpl[Can2 & GetMany, A, B, C, D]                    => composeFold(fold)
      case setter: SetterImpl[Can2 & Modify, A, B, C, D]                 => composeSetter(setter)
      case _                                                                => NullOpticImpl

  override def toString: String =
    "IsoImpl"

end IsoImpl
