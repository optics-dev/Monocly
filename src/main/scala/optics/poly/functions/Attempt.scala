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

  given [A] as Attempt[Option[A]] {

    type Error = NoSuchElementException
    type To = A

    def attempt = {
      EOptional(
        _.map(Right(_)).getOrElse(Left(NoSuchElementException("None is not Some"))),
        to => const(Some(to))
      )
    }
  }

  given [A, B] as Attempt[Either[A, B]] {

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
