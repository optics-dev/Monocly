package monocle.impl

import monocle._
import monocle.internal._

object OpticImpl:
  def composeFold[S, T, A, B, C, D](optic1: FoldImpl[S, T, A, B], optic2: FoldImpl[A, B, C, D]): FoldImpl[S, T, C, D] = 
    new FoldImpl:
      override def foldMap[M: Monoid](f: C => M)(s: S): M = optic1.foldMap(a => optic2.foldMap(f)(a))(s)

  def composeGetter[S, T, A, B, C, D](optic1: GetterImpl[S, T, A, B], optic2: GetterImpl[A, B, C, D]): GetterImpl[S, T, C, D] = 
    new GetterImpl:
      override def get(s: S): C = optic2.get(optic1.get(s))

  def composeOptionalGetter[S, T, A, B, C, D](optic1: OptionalGetterImpl[S, T, A, B], optic2: OptionalGetterImpl[A, B, C, D]): OptionalGetterImpl[S, T, C, D] = 
    new OptionalGetterImpl:
      override def getOption(s: S): Option[C] = optic1.getOption(s).flatMap(optic2.getOption)

  def composeNonEmptyFold[S, T, A, B, C, D](optic1: NonEmptyFoldImpl[S, T, A, B], optic2: NonEmptyFoldImpl[A, B, C, D]): NonEmptyFoldImpl[S, T, C, D] = 
    new NonEmptyFoldImpl:
      override def nonEmptyFoldMap[M: Semigroup](f: C => M)(s: S): M = optic1.nonEmptyFoldMap(a => optic2.nonEmptyFoldMap(f)(a))(s)

  def composeSetter[S, T, A, B, C, D](optic1: SetterImpl[S, T, A, B], optic2: SetterImpl[A, B, C, D]): SetterImpl[S, T, C, D] = 
    new SetterImpl:
      override def modify(f: C => D): S => T = optic1.modify(optic2.modify(f))
      override def replace(d: D): S => T = optic1.modify(optic2.replace(d))

  def composeTraversal[S, T, A, B, C, D](optic1: TraversalImpl[S, T, A, B], optic2: TraversalImpl[A, B, C, D]): TraversalImpl[S, T, C, D] = 
    new TraversalImpl:
      override def modifyA[F[+_]: Applicative](f: C => F[D])(s: S): F[T] =
        optic1.modifyA(a => optic2.modifyA(f)(a))(s)

  def composeNonEmptyTraversal[S, T, A, B, C, D](optic1: NonEmptyTraversalImpl[S, T, A, B], optic2: NonEmptyTraversalImpl[A, B, C, D]): NonEmptyTraversalImpl[S, T, C, D] = 
    new NonEmptyTraversalImpl:
      override def nonEmptyModifyA[F[+_]: Apply](f: C => F[D])(s: S): F[T] =
        optic1.nonEmptyModifyA(a => optic2.nonEmptyModifyA(f)(a))(s)

  def composeOptional[S, T, A, B, C, D](optic1: OptionalImpl[S, T, A, B], optic2: OptionalImpl[A, B, C, D]): OptionalImpl[S, T, C, D] = 
    new OptionalImpl:
      override def getOrModify(s: S): Either[T, C] = 
        optic1
          .getOrModify(s)
          .flatMap(a => optic2.getOrModify(a).left.map(optic1.replace(_)(s)))

      override def replace(d: D): S => T = optic1.modify(optic2.replace(d))
      override def getOption(s: S): Option[C] = optic1.getOption(s) flatMap optic2.getOption
      override def modifyA[F[+_]: Applicative](f: C => F[D])(s: S): F[T] = optic1.modifyA(optic2.modifyA(f))(s)
      override def modify(f: C => D): S => T = optic1.modify(optic2.modify(f))
  end composeOptional

  def composeLens[S, T, A, B, C, D](optic1: LensImpl[S, T, A, B], optic2: LensImpl[A, B, C, D]): LensImpl[S, T, C, D] = 
    new LensImpl:
      override def get(s: S): C = optic2.get(optic1.get(s))
      override def replace(d: D): S => T = optic1.modify(optic2.replace(d))
      override def modifyF[F[+_]: Functor](f: C => F[D])(s: S): F[T] = optic1.modifyF(optic2.modifyF(f))(s)
      override def modify(f: C => D): S => T = optic1.modify(optic2.modify(f))

  def composePrism[S, T, A, B, C, D](optic1: PrismImpl[S, T, A, B], optic2: PrismImpl[A, B, C, D]): PrismImpl[S, T, C, D] = 
    new PrismImpl:
      override def getOrModify(s: S): Either[T, C] = optic1.getOrModify(s)
                                                  .flatMap(a => optic2.getOrModify(a).left.map(optic1.replace(_)(s)))
      override def reverseGet(d: D): T = optic1.reverseGet(optic2.reverseGet(d))
      override def getOption(s: S): Option[C] = optic1.getOption(s) flatMap optic2.getOption

  def composeIso[S, T, A, B, C, D](optic1: IsoImpl[S, T, A, B], optic2: IsoImpl[A, B, C, D]): IsoImpl[S, T, C, D] = 
    new IsoImpl: 
      composeSelf =>

      override def get(s: S): C = optic2.get(optic1.get(s))
      override def reverseGet(d: D): T = optic1.reverseGet(optic2.reverseGet(d))
      override def reverse: IsoImpl[D, C, T, S] =
        new IsoImpl:
          override def get(d: D): T = optic1.reverseGet(optic2.reverseGet(d))
          override def reverseGet(s: S): C = optic2.get(optic1.get(s))
          override def reverse: IsoImpl[S, T, C, D] = composeSelf
  end composeIso

end OpticImpl

sealed trait OpticImpl[-S, +T, +A, -B]:
  def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D]

  def foldMap[M: Monoid](f: A => M)(s: S): M = sys.error("This optic does not support `foldMap`")
  def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = sys.error("This optic does not support `nonEmptyFoldMap`")
  def getOrModify(s: S): Either[T, A] = sys.error("This optic does not support `getOrModify`")
  def getOption(s: S): Option[A] = sys.error("This optic does not support `getOption`")
  def get(s: S): A = sys.error("This optic does not support `get`")
  def replace(b: B): S => T = sys.error("This optic does not support `replace`")
  def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = sys.error("This optic does not support `modifyA`")
  def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] = sys.error("This optic does not support `nonEmptyModifyA`")
  def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T] = sys.error("This optic does not support `modifyF`")
  def modify(f: A => B): S => T = sys.error("This optic does not support `modify`")
  def reverseGet(b: B): T = sys.error("This optic does not support `reverseGet`")
  def reverse: IsoImpl[B, A, T, S] = sys.error("This optic does not support `reverse`")

end OpticImpl


object NullOpticImpl extends OpticImpl[Any, Nothing, Nothing, Any]: 
  override def andThen[C, D](optic2: OpticImpl[Nothing, Any, C, D]): OpticImpl[Any, Nothing, C, D] = this


trait FoldImpl[-S, +T, +A, -B] extends OpticImpl[S, T, A, B]:
  def foldMap[M: Monoid](f: A => M)(s: S): M

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case fold: FoldImpl[A, B, C, D] => OpticImpl.composeFold(this, fold)
    case _ => NullOpticImpl

end FoldImpl


trait SetterImpl[-S, +T, +A, -B] extends OpticImpl[S, T, A, B]:
  def modify(f: A => B): S => T
  def replace(b: B): S => T

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case setter: SetterImpl[A, B, C, D] => OpticImpl.composeSetter(this, setter)
    case _ => NullOpticImpl

end SetterImpl


trait OptionalGetterImpl[-S, +T, +A, -B] extends FoldImpl[S, T, A, B]:
  def getOption(s: S): Option[A]

  override def foldMap[M](f: A => M)(s: S)(using m: Monoid[M]): M = getOption(s).fold(m.empty)(f)

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case getOpt: OptionalGetterImpl[A, B, C, D] => OpticImpl.composeOptionalGetter(this, getOpt)
    case _ => super.andThen(optic2)

end OptionalGetterImpl


trait NonEmptyFoldImpl[-S, +T, +A, -B] extends FoldImpl[S, T, A, B]:
  def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M

  override def foldMap[M: Monoid](f: A => M)(s: S): M = nonEmptyFoldMap(f)(s)

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case fold1: NonEmptyFoldImpl[A, B, C, D] => OpticImpl.composeNonEmptyFold(this, fold1)
    case _ => super.andThen(optic2)

end NonEmptyFoldImpl


trait GetterImpl[-S, +T, +A, -B] extends OptionalGetterImpl[S, T, A, B] with NonEmptyFoldImpl[S, T, A, B]:
  def get(s: S): A

  override def getOption(s: S): Option[A] = Some(get(s))
  override def foldMap[M: Monoid](f: A => M)(s: S): M = f(get(s))
  override def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = f(get(s))

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case getter: GetterImpl[A, B, C, D] => OpticImpl.composeGetter(this, getter)
    case _ => super.andThen(optic2)

end GetterImpl


trait TraversalImpl[-S, +T, +A, -B] extends FoldImpl[S, T, A, B] with SetterImpl[S, T, A, B]:
  def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T]

  override def foldMap[M: Monoid](f: A => M)(s: S): M = modifyA[[x] =>> Const[M, x]](a => Const(f(a)))(s).getConst
  override def modify(f: A => B): S => T = modifyA[Id](f)
  override def replace(b: B): S => T = modify(_ => b)

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case traversal: TraversalImpl[A, B, C, D] => OpticImpl.composeTraversal(this, traversal)
    case _ => super.andThen(optic2)

end TraversalImpl


trait NonEmptyTraversalImpl[-S, +T, +A, -B] extends NonEmptyFoldImpl[S, T, A, B] with TraversalImpl[S, T, A, B]:
  def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T]

  override def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = nonEmptyModifyA[[x] =>> Const[M, x]](a => Const(f(a)))(s).getConst
  override def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = nonEmptyModifyA(f)(s)

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case neTraversal: NonEmptyTraversalImpl[A, B, C, D] => OpticImpl.composeNonEmptyTraversal(this, neTraversal)
    case _ => super.andThen(optic2)

end NonEmptyTraversalImpl


trait OptionalImpl[-S, +T, +A, -B] extends TraversalImpl[S, T, A, B] with OptionalGetterImpl[S, T, A, B]:
  def getOrModify(s: S): Either[T, A]
  def replace(b: B): S => T
  def getOption(s: S): Option[A]

  override def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    getOrModify(s).fold(
      t => Applicative[F].pure(t),
      a => Applicative[F].map(f(a))(b => replace(b)(s)))

  override def modify(f: A => B): S => T = 
    s => getOrModify(s).fold(identity, a => replace(f(a))(s))

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case optional: OptionalImpl[A, B, C, D] => OpticImpl.composeOptional(this, optional)
    case _ => super.andThen(optic2)

end OptionalImpl


trait LensImpl[-S, +T, +A, -B] extends OptionalImpl[S, T, A, B] with NonEmptyTraversalImpl[S, T, A, B] with GetterImpl[S, T, A, B]:
  def get(s: S): A
  def replace(b: B): S => T
  def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T]
  def modify(f: A => B): S => T

  override def getOrModify(s: S): Either[T, A] = Right(get(s))
  override def getOption(s: S): Option[A] = Some(get(s))
  override def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = modifyF(f)(s)
  override def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] = modifyF(f)(s)
  override def foldMap[M: Monoid](f: A => M)(s: S): M = f(get(s))
  override def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = f(get(s))

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case lens: LensImpl[A, B, C, D] => OpticImpl.composeLens(this, lens)
    case _ => super.andThen(optic2)

end LensImpl


trait PrismImpl[-S, +T, +A, -B] extends OptionalImpl[S, T, A, B]:
  def reverseGet(b: B): T

  override def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    getOrModify(s).fold(
      t => Applicative[F].pure(t), 
      a => Applicative[F].map(f(a))(reverseGet))

  override def modify(f: A => B): S => T = s => getOrModify(s).fold(identity, a => reverseGet(f(a)))
  override def replace(b: B): S => T = modify(_ => b)

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case prism: PrismImpl[A, B, C, D] => OpticImpl.composePrism(this, prism)
    case _ => super.andThen(optic2)

end PrismImpl

trait IsoImpl[-S, +T, +A, -B] extends LensImpl[S, T, A, B] with PrismImpl[S, T, A, B]:
  def reverse: IsoImpl[B, A, T, S]

  override def foldMap[M: Monoid](f: A => M)(s: S): M = f(get(s))
  override def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T] = Functor[F].map(f(get(s)))(reverseGet)
  override def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = modifyF(f)(s)
  override def modify(f: A => B): S => T = s => reverseGet(f(get(s)))
  override def replace(b: B): S => T = _ => reverseGet(b)

  override def andThen[C, D](optic2: OpticImpl[A, B, C, D]): OpticImpl[S, T, C, D] = optic2 match 
    case iso: IsoImpl[A, B, C, D] => OpticImpl.composeIso(this, iso)
    case _ => super.andThen(optic2)

end IsoImpl
