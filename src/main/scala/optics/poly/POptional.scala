package optics.poly

import optics.internal.{Applicative, Id}
import optics.poly.functions.Index

trait POptional[S, T, A, B]  { self =>
  def getOrModify(from: S): Either[T, A]

  def replace(to: B): S => T

  def modifyF[F[_] : Applicative](f: A => F[B])(from: S): F[T] =
    getOrModify(from).fold(
      Applicative[F].pure(_),
      a => Applicative[F].map(f(a))(replace(_)(from))
    )

  def modify(f: A => B): S => T =
    modifyF[Id](f)

  def getOption(from: S): Option[A] =
    getOrModify(from).toOption

  def some[A1, B1](implicit ev1: A =:= Option[A1], ev2: Option[B1] =:= B): POptional[S, T, A1, B1] =
    adapt.andThen(PPrism.some[A1, B1])

  def adapt[A1, B1](implicit evA: A =:= A1, evB: B1 =:= B): POptional[S, T, A1, B1] =
    evB.substituteContra[[X] =>> POptional[S, T, A1, X]](
      evA.substituteCo[[X] =>> POptional[S, T, X, B]](this)
    )

  def andThen[E1, C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] =
    new POptional[S, T, C, D] {
      def getOrModify(from: S): Either[T, C] =
        for {
          a <- self.getOrModify(from)
          t <- other.getOrModify(a).left.map(self.replace(_)(from))
        } yield t

      def replace(to: D): S => T =
        self.modify(other.replace(to))
    }

  def andThen[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] = asTraversal.andThen(other)
  def andThen[C, D](other: PPrism[A, B, C, D]): POptional[S, T, C, D] = andThen(other.asOptional)
  def andThen[C, D](other: PLens[A, B, C, D]): POptional[S, T, C, D] = andThen(other.asOptional)
  def andThen[C, D](other: PIso[A, B, C, D]): POptional[S, T, C, D] = andThen(other.asOptional)

  def asTraversal: PTraversal[S, T, A, B] = new PTraversal[S, T, A, B] {
    def modifyF[F[_] : Applicative](f: A => F[B])(from: S): F[T] =
      self.modifyF(f)(from)
  }
}



object POptional {
  extension [From, To, Key,  T] (self: Optional[From, To]) {
    def index(key: Key)(using idx: Index[To, Key] { type To = T}): Optional[From, idx.To] =
      self.andThen(idx.index(key))
  }

  def apply[S, T, A, B](_getOrModify: S => Either[T, A], _replace: B => S => T): POptional[S, T, A, B] =
    new POptional[S, T, A, B] {
      def getOrModify(from: S): Either[T, A] = _getOrModify(from)
      def replace(to: B): S => T = _replace(to)
    }
}

object Optional {
  def apply[From, To](_getOption: From => Option[To], _replace: To => From => From): Optional[From, To] =
    POptional(from => _getOption(from).toRight(from), _replace)
}
