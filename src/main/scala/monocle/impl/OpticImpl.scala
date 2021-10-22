package monocle.impl

import monocle._
import monocle.internal._

private[monocle] trait OpticImpl[+Can, -Structure, +Modified, +Out, -B]:
  self =>

  def andThen[Can2 >: Can, Out2, In2](optic2: OpticImpl[Can2, Out, B, Out2, In2]): OpticImpl[Can2, Structure, Modified, Out2, In2]

  final protected def unsupported(name: String): Nothing =
    sys.error(s"This optic does not support `$name`")

  protected[impl] def get(s: Structure): Out =
    unsupported("get")

  protected[impl] def getOption(s: Structure): Option[Out] =
    unsupported("getOption")

  protected[impl] def getOrModify(s: Structure): Either[Modified, Out] =
    unsupported("getOrModify")

  protected[impl] def toIterator(s: Structure): Iterator[Out] =
    unsupported("toIterator")

  protected[impl] def nonEmptyFoldMap[M: Semigroup](f: Out => M)(s: Structure): M =
    unsupported("nonEmptyFoldMap")

  protected[impl] def nonEmptyModifyA[F[+_]: Apply](f: Out => F[B])(s: Structure): F[Modified] =
    unsupported("nonEmptyModifyA")

  protected[impl] def modify(f: Out => B): Structure => Modified =
    unsupported("modify")

  protected[impl] def modifyA[F[+_]: Applicative](f: Out => F[B])(s: Structure): F[Modified] =
    unsupported("modifyA")

  protected[impl] def modifyF[F[+_]: Functor](f: Out => F[B])(s: Structure): F[Modified] =
    unsupported("modifyF")

  protected[impl] def replace(b: B): Structure => Modified =
    unsupported("replace")

  protected[impl] def reverse: IsoImpl[B, Out, Modified, Structure] =
    unsupported("reverse")

  protected[impl] def reverseGet(b: B): Modified =
    unsupported("reverseGet")

  object safe:
    inline def get(s: Structure)(using Can <:< Get): Out =
      self.get(s)

    inline def getOption(s: Structure)(using Can <:< GetOption): Option[Out] =
      self.getOption(s)

    inline def getOrModify(s: Structure)(using Can <:< (GetOption & Modify)): Either[Modified, Out] =
      self.getOrModify(s)

    inline def toIterator(s: Structure)(using Can <:< GetMany): Iterator[Out] =
      self.toIterator(s)

    inline def nonEmptyFoldMap[M](f: Out => M)(s: Structure)(using Semigroup[M])(using Can <:< GetOneOrMore): M =
      self.nonEmptyFoldMap(f)(s)

    inline def nonEmptyModifyA[F[+_]](f: Out => F[B])(s: Structure)(using Apply[F])(using
      Can <:< (GetOneOrMore & Modify)
    ): F[Modified] =
      self.nonEmptyModifyA(f)(s)

    inline def modify(f: Out => B)(using Can <:< Modify): Structure => Modified =
      self.modify(f)

    inline def modifyA[F[+_]](f: Out => F[B])(s: Structure)(using Applicative[F])(using Can <:< (GetMany & Modify)): F[Modified] =
      self.modifyA(f)(s)

    inline def modifyF[F[+_]](f: Out => F[B])(s: Structure)(using Functor[F])(using Can <:< (Get & Modify)): F[Modified] =
      self.modifyF(f)(s)

    inline def replace(b: B)(using Can <:< Modify): Structure => Modified =
      self.replace(b)

    inline def reverse(using Can <:< Get & ReverseGet): IsoImpl[B, Out, Modified, Structure] =
      self.reverse

    inline def reverseGet(b: B)(using Can <:< ReverseGet): Modified =
      self.reverseGet(b)

end OpticImpl
