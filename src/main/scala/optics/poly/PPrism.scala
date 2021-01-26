package optics.poly

import optics.internal.Applicative
import optics.poly.functions.Index

trait PPrism[S, T, A, B]  { self =>
  def reverseGet(to: B): T

  def getOrModify(from: S): Either[T, A]

  def getOption(from: S): Option[A] = getOrModify(from).toOption

  def replace(to: B): S => T =
    _ => reverseGet(to)

  def andThen[C, D](other: PPrism[A, B, C, D]): PPrism[S, T, C, D] =
    new PPrism[S, T, C, D] {
      def getOrModify(from: S): Either[T, C] =
        for {
          a <- self.getOrModify(from)
          t <- other.getOrModify(a).left.map(self.replace(_)(from))
        } yield t

      def reverseGet(to: D): T =
        self.reverseGet(other.reverseGet(to))
    }

  def andThen[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] = asTraversal.andThen(other)
  def andThen[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] = asOptional.andThen(other)
  def andThen[C, D](other: PLens[A, B, C, D]): POptional[S, T, C, D] = andThen(other.asOptional)
  def andThen[C, D](other: PIso[A, B, C, D]): PPrism[S, T, C, D] = andThen(other.asPrism)

  def asTraversal: PTraversal[S, T, A, B] = new PTraversal[S, T, A, B] {
    def modifyF[F[_] : Applicative](f: A => F[B])(from: S): F[T] =
      self.getOrModify(from).fold(
        Applicative[F].pure(_),
        a => Applicative[F].map(f(a))(self.reverseGet)
      )
  }
  def asOptional: POptional[S, T, A, B] = new POptional[S, T, A, B] {
    def getOrModify(from: S): Either[T, A] = self.getOrModify(from)
    def replace(to: B): S => T = self.replace(to)(_)
  }
}


object PPrism {
  extension [From, To, Key, T] (self: Prism[From, To]) {
    def index(key: Key)(using idx: Index[To, Key] { type To = T}): Optional[From, idx.To] =
      self.andThen(idx.index(key))
  }

  def apply[S, T, A, B](_getOrModify: S => Either[T, A], _reverseGet: B => T): PPrism[S, T, A, B] =
    new PPrism[S, T, A, B] {
    def getOrModify(from: S): Either[T, A] = _getOrModify(from)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def some[A, B]: PPrism[Option[A], Option[B], A, B] =
    apply[Option[A], Option[B], A, B](_.toRight(None), Some(_))
}

object Prism {
  def apply[From, To](_getOption: From => Option[To], _reverseGet: To => From): Prism[From, To] =
    PPrism[From, From, To, To](from => _getOption(from).toRight(from), _reverseGet)

  def some[A]: Prism[Option[A], A] =
    PPrism.some[A, A]

  def partial[From, To](get: PartialFunction[From, To])(reverseGet: To => From): Prism[From, To] =
    apply(from => get.lift(from), reverseGet)

}
