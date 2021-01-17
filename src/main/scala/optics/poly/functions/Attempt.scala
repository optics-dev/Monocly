package optics.poly.functions

import optics.poly.{EOptional, PPrism}
import Function.const

trait Attempt[From] {

  type Error
  type To

  def attempt: EOptional[Error, From, To]

}

object Attempt {

  def apply[From, Err, T](using attempt: Attempt[From] { type Error = Err; type To = T }): EOptional[Err, From, T] =
    attempt.attempt

  given [A]: Attempt[Option[A]] with {

    type Error = NoSuchElementException
    type To = A

    def attempt = {
      EOptional(
        _.map(Right(_)).getOrElse(Left(new NoSuchElementException("None is not Some"))),
        to => const(Some(to))
      )
    }
  }

  given [A, B]:  Attempt[Either[A, B]] with {

    type Error = A
    type To = B

    def attempt = {
      EOptional(
        identity,
        to => const(Right(to))
      )
    }
  }

}
