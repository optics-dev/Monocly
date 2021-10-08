package monocle.impl

import monocle._
import monocle.internal._

trait IsoImpl[-S, +T, +A, -B] 
    extends LensImpl[Get & ReverseGet, S, T, A, B] 
    with PrismImpl[Get & ReverseGet, S, T, A, B]:

  optic1 => 

  protected[impl] def reverse: IsoImpl[B, A, T, S]

  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = 
    f(get(s))

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

  override def andThen[ThatCan >: Get & ReverseGet, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = 
    optic2 match 
      case iso: IsoImpl[A, B, C, D] => composeIso(iso)
      case lens: LensImpl[ThatCan & Get & Modify, A, B, C, D] => composeLens(lens)
      case prism: PrismImpl[ThatCan & GetOption & ReverseGet, A, B, C, D] => composePrism(prism)
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
    "IsoImpl"

end IsoImpl
