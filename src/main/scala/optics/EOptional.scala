package optics

import optics.internal.{Applicative, TraversalRes}
import optics.poly.EPOptional

import scala.annotation.alpha

/**
 * Minimum implementation: `getOrError` and `replace`
 */
trait EOptional[+Error, From, To] extends EPOptional[Error, From, From, To, To] with ETraversal[Error, From, To] { self =>
  def getOrModify(from: From): Either[(Error, From), To] =
    getOrError(from).left.map(_ -> from)

  override def traversal[F[+ _] : Applicative](f: To => F[To])(from: From): TraversalRes[F, Error, From] =
    getOrError(from) match {
      case Left(e)   => TraversalRes.Failure(e, Applicative[F].pure(from))
      case Right(to) => TraversalRes.Success(Applicative[F].map(f(to))(replace(_)(from)))
    }

  @alpha("andThen")
  def >>>[NewError >: Error, Next](other: EOptional[NewError, To, Next]): EOptional[NewError, From, Next] =
    new EOptional[NewError, From, Next] {
      override def getOrError(from: From): Either[NewError, Next] =
        self.getOrError(from).flatMap(other.getOrError)

      override def replace(to: Next): From => From =
        self.modify(other.replace(to))
    }
}

object EOptional {
  def apply[Error, From, To](_getOrError: From => Either[Error, To], _replace: To => From => From): EOptional[Error, From, To] =
    new EOptional[Error, From, To] {
      override def getOrError(from: From): Either[Error, To] = _getOrError(from)
      override def replace(to: To): From => From = _replace(to)
    }
}

object Optional {
  def apply[A, B](_getOrError: A => Either[BasicError, B], _replace: B => A => A): Optional[A, B] =
    EOptional(_getOrError, _replace)
}

