package optics.poly

import optics.internal.Applicative
import optics.poly.functions.Index

trait PLens[S, T, A, B] { self =>
  def get(from: S): A
  def replace(to: B): S => T

  def modify(f: A => B): S => T =
    from => replace(f(get(from)))(from)

  def some[A1, B1](implicit ev1: A =:= Option[A1], ev2: Option[B1] =:= B): POptional[S, T, A1, B1] =
    adapt.andThen(PPrism.some[A1, B1])

  def adapt[A1, B1](implicit evA: A =:= A1, evB: B1 =:= B): PLens[S, T, A1, B1] =
    evB.substituteContra[[X] =>> PLens[S, T, A1, X]](
    evA.substituteCo[[X] =>> PLens[S, T, X, B]](this)
  )

  def andThen[C, D](other: PLens[A, B, C, D]): PLens[S, T, C, D] = new PLens[S, T, C, D] {
    def get(from: S): C = other.get(self.get(from))
    def replace(to: D): S => T = self.modify(other.replace(to))
  }

  def andThen[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] = asTraversal.andThen(other)
  def andThen[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] = asOptional.andThen(other)
  def andThen[C, D](other: PPrism[A, B, C, D]): POptional[S, T, C, D] = andThen(other.asOptional)
  def andThen[C, D](other: PIso[A, B, C, D]): PLens[S, T, C, D] = andThen(other.asLens)

  def asTraversal: PTraversal[S, T, A, B] = new PTraversal[S, T, A, B] {
    def modifyF[F[_] : Applicative](f: A => F[B])(from: S): F[T] =
      Applicative[F].map(f(self.get(from)))(self.replace(_)(from))
  }
  def asOptional: POptional[S, T, A, B] = new POptional[S, T, A, B] {
    def getOrModify(from: S): Either[T, A] = Right(self.get(from))
    def replace(to: B): S => T = self.replace(to)(_)
  }
}

object PLens {
  extension [From, To, Key,  T] (self: Lens[From, To]) {
    def index(key: Key)(using idx: Index[To, Key] { type To = T}): Optional[From, idx.To] =
      self.andThen(idx.index(key))
  }

  def apply[S, T, A, B](_get: S => A, _replace: B => S => T): PLens[S, T, A, B] = new PLens[S, T, A, B] {
    def get(from: S): A = _get(from)
    def replace(to: B): S => T = _replace(to)
  }

  def _1[A1, A2, B]: PLens[(A1, A2), (B, A2), A1, B] =
    apply[(A1, A2), (B, A2), A1, B](_._1, newValue => _.copy(_1 = newValue))

  def _2[A1, A2, B]: PLens[(A1, A2), (A1, B), A2, B] =
    apply[(A1, A2), (A1, B), A2, B](_._2, newValue => _.copy(_2 = newValue))
}


object Lens {

  def id[A]: Lens[A,A] = apply(a => a, _ => a => a)

  def apply[From, To](_get: From => To, _replace: To => From => From): Lens[From, To] =
    PLens(_get, _replace)

  def _1[A1, A2]: Lens[(A1, A2), A1] =
    PLens._1

  def _2[A1, A2]: Lens[(A1, A2), A2] =
    PLens._2
}