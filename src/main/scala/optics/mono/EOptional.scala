package optics.mono

import optics.internal.{Applicative, TraversalRes}

import scala.annotation.alpha

/**
 * Minimum implementation: `getOrError` and `replace`
 */
trait EOptional[+Error, From, To] extends ETraversal[Error, From, To] { self =>
  def getOrError(from: From): Either[Error, To]

  def traversal[F[+_] : Applicative, NewError >: Error](f: To => TraversalRes[F, NewError, To])(from: From): TraversalRes[F, NewError, From] =
    getOrError(from) match {
      case Left(e)   => TraversalRes.Failure(e, Applicative[F].pure(from))
      case Right(to) => f(to).map(replace(_)(from))
    }

  def getOption(from: From): Option[To] =
    getOrError(from).toOption

  def get(from: From)(implicit ev: Error <:< Nothing): To =
    getOrError(from).getOrElse(???)

  def imap[B](f: To => B)(g: B => To): EOptional[Error, From, B] =
    EOptional(getOrError.andThen(_.map(f)), g.andThen(replace))

  @alpha("andThen")
  def >>>[NewError, Next](other: EOptional[NewError, To, Next]): EOptional[Error | NewError, From, Next] =
    new EOptional[Error | NewError, From, Next] {
      override def getOrError(from: From): Either[Error | NewError, Next] =
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
  def apply[A, B](_getOption: A => Option[B], _replace: B => A => A): Optional[A, B] =
    EOptional(_getOption(_).toRight(defaultError), _replace)
}
