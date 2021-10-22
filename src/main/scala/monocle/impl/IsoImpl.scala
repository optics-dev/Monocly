package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait IsoImpl[-Structure, +Modified, +Out, -B]
    extends LensImpl[Get & ReverseGet, Structure, Modified, Out, B]
    with PrismImpl[Get & ReverseGet, Structure, Modified, Out, B]:

  optic1 =>

  protected[impl] def reverse: IsoImpl[B, Out, Modified, Structure]

  override protected[impl] def toIterator(s: Structure): Iterator[Out] =
    Iterator.single(get(s))

  override protected[impl] def modifyF[F[+_]: Functor](f: Out => F[B])(s: Structure): F[Modified] =
    Functor[F].map(f(get(s)))(reverseGet)

  override protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[B])(s: Structure): F[Modified] =
    modifyF(f)(s)

  override protected[impl] def modify(f: Out => B): Structure => Modified =
    s => reverseGet(f(get(s)))

  override protected[impl] def replace(b: B): Structure => Modified =
    _ => reverseGet(b)

  protected def composeIso[Out2, In2](optic2: IsoImpl[Out, B, Out2, In2]): IsoImpl[Structure, Modified, Out2, In2] =
    new IsoImpl:
      composeSelf =>
      override def get(s: Structure): Out2 =
        optic2.get(optic1.get(s))

      override def reverseGet(d: In2): Modified =
        optic1.reverseGet(optic2.reverseGet(d))

      override def reverse: IsoImpl[In2, Out2, Modified, Structure] =
        new IsoImpl:
          override def get(d: In2): Modified =
            optic1.reverseGet(optic2.reverseGet(d))

          override def reverseGet(s: Structure): Out2 =
            optic2.get(optic1.get(s))

          override def reverse: IsoImpl[Structure, Modified, Out2, In2] =
            composeSelf

      end reverse

  end composeIso

  override def andThen[Can2 >: Get & ReverseGet, Out2, In2](
    optic2: OpticImpl[Can2, Out, B, Out2, In2]
  ): OpticImpl[Can2, Structure, Modified, Out2, In2] =
    optic2 match
      case iso: IsoImpl[Out, B, Out2, In2]                                         => composeIso(iso)
      case lens: LensImpl[Can2 & Get & Modify, Out, B, Out2, In2]               => composeLens(lens)
      case prism: PrismImpl[Can2 & GetOption & ReverseGet, Out, B, Out2, In2]   => composePrism(prism)
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
    "IsoImpl"

end IsoImpl
