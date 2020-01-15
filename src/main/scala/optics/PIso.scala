package optics

import scala.annotation.alpha

trait PIso[-S, +T, +A, -B] extends PLens[S, T, A, B] with EPPrism[Nothing, S, T, A, B] { self =>
  def reverse: PIso[B, A, T, S] = PIso(reverseGet)(get)

  @alpha("andThen")
  def >>>[C, D](other: PIso[A, B, C, D]): PIso[S, T, C, D] = new PIso[S, T, C, D] {
    def get(from: S): C      = other.get(self.get(from))
    def reverseGet(to: D): T = self.reverseGet(other.reverseGet(to))
  }
}

object PIso {
  def apply[S, T, A, B](_get: S => A)(_reverseGet: B => T): PIso[S, T, A, B] = new PIso[S, T, A, B] {
    def get(from: S): A = _get(from)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def id[A, B]: PIso[A, B, A, B] =
    PIso[A, B, A, B](identity)(identity)
}

object Iso {
  def apply[A, B](_get: A => B)(_reverseGet: B => A): Iso[A, B] =
    PIso(_get)(_reverseGet)

  def id[A]: Iso[A, A] =
    PIso.id
}