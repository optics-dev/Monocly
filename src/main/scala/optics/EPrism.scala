package optics

import optics.poly.EPPrism

import scala.annotation.alpha

/**
 * Minimum implementation: `getOrError` and `reverseGet`
 */
trait EPrism[+Error, From, To] extends EPPrism[Error, From, From, To, To] with EOptional[Error, From, To] { self =>
  override def getOrModify(from: From): Either[(Error, From), To] =
    getOrError(from).left.map(_ -> from)

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

  def some[E, A](error: E): EPrism[E, Option[A], A] =
    EPrism.some(error)
}

object Prism {
  def apply[A, B](_getOError: A => Either[BasicError, B], _reverseGet: B => A): Prism[A, B] =
    EPrism(_getOError, _reverseGet)

  def some[A]: Prism[Option[A], A] =
    EPrism.some("None is not a Some")
}


