package optics.poly

import optics.BasicError

import scala.annotation.alpha

trait EPPrism[+E, -S, +T, +A, -B] extends EPOptional[E, S, T, A, B] { self =>
  def reverseGet(to: B): T

  override def replace(to: B): S => T =
    _ => reverseGet(to)

  @alpha("andThen")
  def >>>[E1 >: E, C, D](other: EPPrism[E1, A, B, C, D]): EPPrism[E1, S, T, C, D] = new EPPrism[E1, S, T, C, D] {
    def getOrModify(from: S): Either[(E1, T), C] =
      for {
        a <- self.getOrModify(from)
        t <- other.getOrModify(a).left.map{ case (e1, b) => (e1, self.replace(b)(from)) }
      } yield t
    def reverseGet(to: D): T = self.reverseGet(other.reverseGet(to))
  }
}

object EPPrism {
  def apply[E, S, T, A, B](_getOrModify: S => Either[(E, T), A])(_reverseGet: B => T): EPPrism[E, S, T, A, B] = new EPPrism[E, S, T, A, B] {
    def getOrModify(from: S): Either[(E, T), A] = _getOrModify(from)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def some[E, A, B](error: E): EPPrism[E, Option[A], Option[B], A, B] =
    apply[E, Option[A], Option[B], A, B](_.toRight((error, None)))(Some(_))
}

object PPrism {
  def apply[S, T, A, B](_getOrModify: S => Either[(BasicError, T), A])(_reverseGet: B => T): PPrism[S, T, A, B] = new PPrism[S, T, A, B] {
    def getOrModify(from: S): Either[(BasicError, T), A] = _getOrModify(from)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def some[A, B]: PPrism[Option[A], Option[B], A, B] =
    EPPrism.some("None is not a Some")
}