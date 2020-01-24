package optics

import scala.annotation.alpha

/**
 * Minimum implementation: `getOrError` and `reverseGet`
 */
trait EPrism[+Error, From, To] extends EOptional[Error, From, To] { self =>
  def reverseGet(next: To): From

  override def replace(to: To): From => From =
    _ => reverseGet(to)

  @alpha("andThen")
  def >>>[NewError >: Error, Next](other: EPrism[NewError, To, Next]): EPrism[NewError, From, Next] =
    new EPrism[NewError, From, Next] {
      override def getOrError(from: From): Either[NewError, Next] =
        self.getOrError(from).flatMap(other.getOrError)

      def reverseGet(to: Next): From =
        self.reverseGet(other.reverseGet(to))
    }
}

object EPrism {
  def apply[Error, From, To](_getOError: From => Either[Error, To], _reverseGet: To => From): EPrism[Error, From, To] =
    new EPrism[Error, From, To] {
      override def getOrError(from: From): Either[Error, To] = _getOError(from)
      def reverseGet(to: To): From = _reverseGet(to)
    }

  def partial[Error, From, To](get: PartialFunction[From, To])(mismatch: From => Error)(reverseGet: To => From): EPrism[Error, From, To] =
    apply(from => get.lift(from).toRight(mismatch(from)), reverseGet)

  def some[Error, A](error: Error): EPrism[Error, Option[A], A] =
    partial[Error, Option[A], A]{ case Some(a) => a }(_ => error)(Some(_))
}

object Prism {
  def apply[A, B](_getOption: A => Option[B], _reverseGet: B => A): Prism[A, B] =
    EPrism(_getOption(_).toRight(defaultError), _reverseGet)

  def some[A]: Prism[Option[A], A] =
    EPrism.some("None is not a Some")
}


