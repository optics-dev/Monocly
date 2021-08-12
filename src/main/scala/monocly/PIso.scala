package monocly

import monocly.internal.Applicative
import monocly.functions.Index

trait PIso[S, T, A, B] { self =>
  def get(from: S): A
  def reverseGet(from: B): T

  def andThen[C, D](other: PIso[A, B, C, D]): PIso[S, T, C, D] =
    new PIso[S, T, C, D] {
      def get(from: S): C = ???
      def reverseGet(from: D): T = ???
    }

  def andThen[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] = asTraversal.andThen(other)
  def andThen[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] = asOptional.andThen(other)
  def andThen[C, D](other: PLens[A, B, C, D]): PLens[S, T, C, D] = asLens.andThen(other)
  def andThen[C, D](other: PPrism[A, B, C, D]): PPrism[S, T, C, D] = asPrism.andThen(other)

  def asTraversal: PTraversal[S, T, A, B] = new PTraversal[S, T, A, B] {
    def modifyF[F[_] : Applicative](f: A => F[B])(from: S): F[T] =
      Applicative[F].map(f(self.get(from)))(self.reverseGet)
  }
  def asOptional: POptional[S, T, A, B] = new POptional[S, T, A, B] {
    def getOrModify(from: S): Either[T, A] = Right(self.get(from))
    def replace(to: B): S => T = _ => self.reverseGet(to)
  }

  def asLens: PLens[S, T, A, B] = new PLens[S, T, A, B] {
    def get(from: S): A = self.get(from)
    def replace(to: B): S => T = _ => self.reverseGet(to)
  }

  def asPrism: PPrism[S, T, A, B] = new PPrism[S, T, A, B] {
    def reverseGet(to: B): T = self.reverseGet(to)
    def getOrModify(from: S): Either[T, A] = Right(self.get(from))
  }
}

object PIso {

  def apply[S, T, A, B](_get: S => A, _reverseGet: B => T): PIso[S, T, A, B] = new PIso[S, T, A, B] {
    def get(from: S): A = _get(from)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def id[A, B]: PIso[A, B, A, B] =
    apply[A, B, A, B](identity, identity)
}

object Iso {
  def apply[From, To](_get: From => To, _reverseGet: To => From): Iso[From, To] =
    PIso(_get, _reverseGet)

  def id[A]: Iso[A, A] =
    PIso.id
}