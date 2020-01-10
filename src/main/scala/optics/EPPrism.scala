package optics

import scala.annotation.alpha

trait EPPrism[+E, -S, +T, +A, -B] extends EPOptional[E, S, T, A, B] { self =>
  def reverseGet(to: B): T

  def set(to: B): S => T = _ => reverseGet(to)

  override def modify(f: A => B): S => T = from =>
    getOrModify(from).fold(_._2, a => reverseGet(f(a)))

  @alpha("andThen")
  def >>>[E1 >: E, C, D](other: EPPrism[E1, A, B, C, D]): EPPrism[E1, S, T, C, D] = new EPPrism[E1, S, T, C, D] {
    def getOrModify(from: S): Either[(E1, T), C] =
      for {
        a <- self.getOrModify(from)
        t <- other.getOrModify(a).left.map{ case (e1, b) => (e1, self.set(b)(from)) }
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
  def apply[S, T, A, B](_getOrModify: S => Either[T, A])(_reverseGet: B => T): PPrism[S, T, A, B] = new PPrism[S, T, A, B] {
    def getOrModify(from: S): Either[(Unit, T), A] = _getOrModify(from).left.map(() -> _)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def some[A, B]: PPrism[Option[A], Option[B], A, B] = EPPrism.some(())
}

object EPrism {
  def apply[E, A, B](_getOError: A => Either[E, B])(_reverseGet: B => A): EPrism[E, A, B] =
    EPPrism[E, A, A, B, B](from => _getOError(from).left.map(_ -> from))(_reverseGet)

  def partial[E, A, B](get: PartialFunction[A, B])(mismatch: A => E)(reverseGet: B => A): EPrism[E, A, B] =
    apply[E, A, B](from => get.lift(from).toRight(mismatch(from)))(reverseGet)

  def some[E, A](error: E): EPrism[E, Option[A], A] = EPPrism.some(error)
}

object Prism {
  def apply[A, B](_getOption: A => Option[B])(_reverseGet: B => A): Prism[A, B] =
    PPrism[A, A, B, B](from => _getOption(from).toRight(from))(_reverseGet)

  def partial[A, B](get: PartialFunction[A, B])(reverseGet: B => A): Prism[A, B] =
    apply(get.lift)(reverseGet)

  def some[A]: Prism[Option[A], A] = PPrism.some[A, A]
}