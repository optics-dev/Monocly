package monocle.impl

import monocle._
import monocle.internal._
 
sealed trait OpticImpl[+ThisCan, -S, +T, +A, -B]:
  self => 

  def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D]

  private def unsupported(name: String): Nothing = 
    sys.error(s"This optic does not support `$name`")

  protected[impl] def get(s: S): A = unsupported("get")
  protected[impl] def getOption(s: S): Option[A] = unsupported("getOption")
  protected[impl] def getOrModify(s: S): Either[T, A] = unsupported("getOrModify")
  protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = unsupported("foldMap")
  protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = unsupported("nonEmptyFoldMap")
  protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] = unsupported("nonEmptyModifyA")
  protected[impl] def modify(f: A => B): S => T = unsupported("modify")
  protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = unsupported("modifyA")
  protected[impl] def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T] = unsupported("modifyF")
  protected[impl] def replace(b: B): S => T = unsupported("replace")
  protected[impl] def reverse: IsoImpl[B, A, T, S] = unsupported("reverse")
  protected[impl] def reverseGet(b: B): T = unsupported("reverseGet")

  object safe:  
    inline def get(s: S)(using ThisCan <:< Get): A = self.get(s)
    inline def getOption(s: S)(using ThisCan <:< GetOption): Option[A] = self.getOption(s)
    inline def getOrModify(s: S)(using ThisCan <:< (GetOption & Modify)): Either[T, A] = self.getOrModify(s)
    inline def foldMap[M](f: A => M)(s: S)(using Monoid[M])(using ThisCan <:< GetMany): M = self.foldMap(f)(s)
    inline def nonEmptyFoldMap[M](f: A => M)(s: S)(using Semigroup[M])(using ThisCan <:< GetOneOrMore): M = self.nonEmptyFoldMap(f)(s)
    inline def nonEmptyModifyA[F[+_]](f: A => F[B])(s: S)(using Apply[F])(using ThisCan <:< (GetOneOrMore & Modify)): F[T] = self.nonEmptyModifyA(f)(s)
    inline def modify(f: A => B)(using ThisCan <:< Modify): S => T = self.modify(f)
    inline def modifyA[F[+_]](f: A => F[B])(s: S)(using Applicative[F])(using ThisCan <:< (GetMany & Modify)): F[T] = self.modifyA(f)(s)
    inline def modifyF[F[+_]](f: A => F[B])(s: S)(using Functor[F])(using ThisCan <:< (Get & Modify)): F[T] = self.modifyF(f)(s)
    inline def replace(b: B)(using ThisCan <:< Modify): S => T = self.replace(b)
    inline def reverse(using ThisCan <:< Get & ReverseGet): IsoImpl[B, A, T, S] = self.reverse
    inline def reverseGet(b: B)(using ThisCan <:< ReverseGet): T = self.reverseGet(b)

end OpticImpl


object NullOpticImpl extends OpticImpl[Nothing, Any, Nothing, Nothing, Any]: 
  override def andThen[ThatCan >: Nothing, C, D](optic2: OpticImpl[ThatCan, Nothing, Any, C, D]): OpticImpl[Nothing, Any, Nothing, C, D] = this
  override def toString: String = "NullOpticImpl"


trait FoldImpl[+ThisCan <: GetMany, -S, +T, +A, -B] extends OpticImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M

  protected def composeFold[ThatCan >: ThisCan <: GetMany, C, D](optic2: FoldImpl[ThatCan, A, B, C, D]): FoldImpl[ThatCan, S, T, C, D] = 
    new FoldImpl:
      override def foldMap[M: Monoid](f: C => M)(s: S): M = optic1.foldMap(a => optic2.foldMap(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case _ => NullOpticImpl

  override def toString: String = "FoldImpl"

end FoldImpl


trait SetterImpl[+ThisCan <: Modify, -S, +T, +A, -B] extends OpticImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def modify(f: A => B): S => T
  protected[impl] def replace(b: B): S => T

  protected def composeSetter[ThatCan >: ThisCan <: Modify, C, D](optic2: SetterImpl[ThatCan, A, B, C, D]): SetterImpl[ThatCan, S, T, C, D] = 
    new SetterImpl:
      override def modify(f: C => D): S => T = optic1.modify(optic2.modify(f))
      override def replace(d: D): S => T = optic1.modify(optic2.replace(d))

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
    case _ => NullOpticImpl

  override def toString: String = "SetterImpl"

end SetterImpl


trait OptionalGetterImpl[+ThisCan <: GetOption, -S, +T, +A, -B] extends FoldImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def getOption(s: S): Option[A]

  override protected[impl] def foldMap[M](f: A => M)(s: S)(using m: Monoid[M]): M = getOption(s).fold(m.empty)(f)

  protected def composeOptionalGetter[ThatCan >: ThisCan <: GetOption, C, D](optic2: OptionalGetterImpl[ThatCan, A, B, C, D]): OptionalGetterImpl[ThatCan, S, T, C, D] = 
    new OptionalGetterImpl:
      override def getOption(s: S): Option[C] = optic1.getOption(s).flatMap(optic2.getOption)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case _ => NullOpticImpl

  override def toString: String = "OptionalGetterImpl"

end OptionalGetterImpl


trait NonEmptyFoldImpl[+ThisCan <: GetOneOrMore, -S, +T, +A, -B] extends FoldImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M

  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = nonEmptyFoldMap(f)(s)

  protected def composeNonEmptyFold[ThatCan >: ThisCan <: GetOneOrMore, C, D](optic2: NonEmptyFoldImpl[ThatCan, A, B, C, D]): NonEmptyFoldImpl[ThatCan, S, T, C, D] = 
    new NonEmptyFoldImpl:
      override def nonEmptyFoldMap[M: Semigroup](f: C => M)(s: S): M = optic1.nonEmptyFoldMap(a => optic2.nonEmptyFoldMap(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case neFold: NonEmptyFoldImpl[ThatCan & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case _ => NullOpticImpl

  override def toString: String = "NonEmptyFoldImpl"

end NonEmptyFoldImpl


trait GetterImpl[+ThisCan <: Get, -S, +T, +A, -B] extends OptionalGetterImpl[ThisCan, S, T, A, B] with NonEmptyFoldImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def get(s: S): A

  override protected[impl] def getOption(s: S): Option[A] = Some(get(s))
  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = f(get(s))
  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = f(get(s))

  protected def composeGetter[ThatCan >: ThisCan <: Get, C, D](optic2: GetterImpl[ThatCan, A, B, C, D]): GetterImpl[ThatCan, S, T, C, D] = 
    new GetterImpl:
      override def get(s: S): C = optic2.get(optic1.get(s))

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case getter: GetterImpl[ThatCan & Get, A, B, C, D] => composeGetter(getter)
    case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
    case neFold: NonEmptyFoldImpl[ThatCan & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case _ => NullOpticImpl

  override def toString: String = "GetterImpl"

end GetterImpl


trait TraversalImpl[+ThisCan <: GetMany & Modify, -S, +T, +A, -B] extends FoldImpl[ThisCan, S, T, A, B] with SetterImpl[ThisCan, S, T, A, B]:
  optic1 =>   

  protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T]

  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = modifyA[[x] =>> Const[M, x]](a => Const(f(a)))(s).getConst
  override protected[impl] def modify(f: A => B): S => T = modifyA[Id](f)
  override protected[impl] def replace(b: B): S => T = modify(_ => b)

  protected def composeTraversal[ThatCan >: ThisCan <: GetMany & Modify, C, D](optic2: TraversalImpl[ThatCan, A, B, C, D]): TraversalImpl[ThatCan, S, T, C, D] = 
    new TraversalImpl:
      override def modifyA[F[+_]: Applicative](f: C => F[D])(s: S): F[T] =
        optic1.modifyA(a => optic2.modifyA(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
    case _ => NullOpticImpl

  override def toString: String = "TraversalImpl"

end TraversalImpl


trait NonEmptyTraversalImpl[+ThisCan <: GetOneOrMore & Modify, -S, +T, +A, -B] extends NonEmptyFoldImpl[ThisCan, S, T, A, B] with TraversalImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T]

  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = nonEmptyModifyA[[x] =>> Const[M, x]](a => Const(f(a)))(s).getConst
  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = nonEmptyModifyA(f)(s)

  protected def composeNonEmptyTraversal[ThatCan >: ThisCan <: GetOneOrMore & Modify, C, D](optic2: NonEmptyTraversalImpl[ThatCan, A, B, C, D]): NonEmptyTraversalImpl[ThatCan, S, T, C, D] = 
    new NonEmptyTraversalImpl:
      override def nonEmptyModifyA[F[+_]: Apply](f: C => F[D])(s: S): F[T] =
        optic1.nonEmptyModifyA(a => optic2.nonEmptyModifyA(f)(a))(s)

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case neTraversal: NonEmptyTraversalImpl[ThatCan & GetOneOrMore & Modify, A, B, C, D] => composeNonEmptyTraversal(neTraversal)
    case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
    case neFold: NonEmptyFoldImpl[ThatCan & GetOneOrMore, A, B, C, D] => composeNonEmptyFold(neFold)
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
    case _ => NullOpticImpl

  override def toString: String = "NonEmptyTraversalImpl"

end NonEmptyTraversalImpl


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

      override def replace(d: D): S => T = optic1.modify(optic2.replace(d))
      override def getOption(s: S): Option[C] = optic1.getOption(s) flatMap optic2.getOption
      override def modifyA[F[+_]: Applicative](f: C => F[D])(s: S): F[T] = optic1.modifyA(optic2.modifyA(f))(s)
      override def modify(f: C => D): S => T = optic1.modify(optic2.modify(f))
  end composeOptional

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case optional: OptionalImpl[ThatCan & GetOption & Modify, A, B, C, D] => composeOptional(optional)
    case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
    case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
    case _ => NullOpticImpl

  override def toString: String = "OptionalImpl"

end OptionalImpl


trait LensImpl[+ThisCan <: Get & Modify, -S, +T, +A, -B] 
    extends OptionalImpl[ThisCan, S, T, A, B] 
    with NonEmptyTraversalImpl[ThisCan, S, T, A, B] 
    with GetterImpl[ThisCan, S, T, A, B]:

  optic1 => 

  protected[impl] def get(s: S): A
  protected[impl] def replace(b: B): S => T
  protected[impl] def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T]
  protected[impl] def modify(f: A => B): S => T

  override protected[impl] def getOrModify(s: S): Either[T, A] = Right(get(s))
  override protected[impl] def getOption(s: S): Option[A] = Some(get(s))
  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = modifyF(f)(s)
  override protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: A => F[B])(s: S): F[T] = modifyF(f)(s)
  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = f(get(s))
  override protected[impl] def nonEmptyFoldMap[M: Semigroup](f: A => M)(s: S): M = f(get(s))

  protected def composeLens[ThatCan >: ThisCan <: Get & Modify, C, D](optic2: LensImpl[ThatCan, A, B, C, D]): LensImpl[ThatCan, S, T, C, D] = 
    new LensImpl:
      override def get(s: S): C = optic2.get(optic1.get(s))
      override def replace(d: D): S => T = optic1.modify(optic2.replace(d))
      override def modifyF[F[+_]: Functor](f: C => F[D])(s: S): F[T] = optic1.modifyF(optic2.modifyF(f))(s)
      override def modify(f: C => D): S => T = optic1.modify(optic2.modify(f))

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
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

  override def toString: String = "LensImpl"

end LensImpl


trait PrismImpl[+ThisCan <: GetOption & ReverseGet, -S, +T, +A, -B] extends OptionalImpl[ThisCan, S, T, A, B]:
  optic1 => 

  protected[impl] def reverseGet(b: B): T

  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] =
    getOrModify(s).fold(
      t => Applicative[F].pure(t), 
      a => Applicative[F].map(f(a))(reverseGet))

  override protected[impl] def modify(f: A => B): S => T = s => getOrModify(s).fold(identity, a => reverseGet(f(a)))
  override protected[impl] def replace(b: B): S => T = modify(_ => b)

  protected def composePrism[ThatCan >: ThisCan <: GetOption & ReverseGet, C, D](optic2: PrismImpl[ThatCan, A, B, C, D]): PrismImpl[ThatCan, S, T, C, D] = 
    new PrismImpl:
      override def getOrModify(s: S): Either[T, C] = optic1.getOrModify(s)
                                                  .flatMap(a => optic2.getOrModify(a).left.map(optic1.replace(_)(s)))
      override def reverseGet(d: D): T = optic1.reverseGet(optic2.reverseGet(d))
      override def getOption(s: S): Option[C] = optic1.getOption(s) flatMap optic2.getOption

  override def andThen[ThatCan >: ThisCan, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
    case prism: PrismImpl[ThatCan & GetOption & ReverseGet, A, B, C, D] => composePrism(prism)
    case optional: OptionalImpl[ThatCan & GetOption & Modify, A, B, C, D] => composeOptional(optional)
    case traversal: TraversalImpl[ThatCan & GetMany & Modify, A, B, C, D] => composeTraversal(traversal)
    case getOpt: OptionalGetterImpl[ThatCan & GetOption, A, B, C, D] => composeOptionalGetter(getOpt)
    case fold: FoldImpl[ThatCan & GetMany, A, B, C, D] => composeFold(fold)
    case setter: SetterImpl[ThatCan & Modify, A, B, C, D] => composeSetter(setter)
    case _ => NullOpticImpl

  override def toString: String = "PrismImpl"

end PrismImpl

trait IsoImpl[-S, +T, +A, -B] 
    extends LensImpl[Get & ReverseGet, S, T, A, B] 
    with PrismImpl[Get & ReverseGet, S, T, A, B]:

  optic1 => 

  protected[impl] def reverse: IsoImpl[B, A, T, S]

  override protected[impl] def foldMap[M: Monoid](f: A => M)(s: S): M = f(get(s))
  override protected[impl] def modifyF[F[+_]: Functor](f: A => F[B])(s: S): F[T] = Functor[F].map(f(get(s)))(reverseGet)
  override protected[impl] def modifyA[F[+_]: Applicative](f: A => F[B])(s: S): F[T] = modifyF(f)(s)
  override protected[impl] def modify(f: A => B): S => T = s => reverseGet(f(get(s)))
  override protected[impl] def replace(b: B): S => T = _ => reverseGet(b)

  protected def composeIso[C, D](optic2: IsoImpl[A, B, C, D]): IsoImpl[S, T, C, D] = 
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

  override def andThen[ThatCan >: Get & ReverseGet, C, D](optic2: OpticImpl[ThatCan, A, B, C, D]): OpticImpl[ThatCan, S, T, C, D] = optic2 match 
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

  override def toString: String = "IsoImpl"

end IsoImpl
