package monocle.std

import monocle._
import monocle.classic.{PPrism, Prism}

object either:
  final def pStdLeft[A, B, C]: PPrism[Either[A, B], Either[C, B], A, C] =
    PPrism[Either[A, B], Either[C, B], A, C] {
      case Left(a)  => Right(a)
      case Right(b) => Left(Right(b))
    }(Left.apply)

  final def stdLeft[A, B]: Prism[Either[A, B], A] =
    pStdLeft[A, B, A]

  final def pStdRight[A, B, C]: PPrism[Either[A, B], Either[A, C], B, C] =
    PPrism[Either[A, B], Either[A, C], B, C] {
      case Left(a)  => Left(Left(a))
      case Right(b) => Right(b)
    }(Right.apply)
