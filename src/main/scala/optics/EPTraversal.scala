package optics

import optics.internal.{Applicative, Id, Proxy, TraversalRes}

import scala.annotation.alpha

trait EPTraversal[+E, -S, +T, +A, -B] { self =>
  def modifyFAndOptError[F[+_]: Applicative](f: A => F[B])(from: S): TraversalRes[F, E, T]

  def modifyFOrError[F[+_]: Applicative](f: A => F[B])(from: S): Either[E, F[T]] =
    modifyFAndOptError(f)(from) match {
      case TraversalRes(Some(e), _  ) => Left(e)
      case TraversalRes(_      , res) => Right(res)
    }

  def modifyOrError(f: A => B): S => Either[E, T] =
    modifyFOrError[Id](f)

  def replaceOrError(to: B): S => Either[E, T] =
    modifyOrError(_ => to)

  def modifyF[F[+_]: Applicative](f: A => F[B])(from: S): F[T] =
    modifyFAndOptError(f)(from).effect

  def modify(f: A => B): S => T =
    modifyF[Id](f)

  def replace(to: B): S => T =
    modify(_ => to)

  def toListOrError(from: S): Either[E, List[A]] = {
    var acc = List.empty[A]
    modifyFOrError{ a =>
      acc = a :: acc
      Proxy.nothing
    }(from).map(_ => acc.reverse)
  }

  def toList(from: S): List[A] =
    toListOrError(from).getOrElse(Nil)

  def iterator(from: S): Iterator[A] =
    toListOrError(from).fold(_ => Iterator.empty, _.iterator)

  @alpha("andThen")
  def >>>[E1 >: E, C, D](other: EPTraversal[E1, A, B, C, D]): EPTraversal[E1, S, T, C, D] =
    new EPTraversal[E1, S, T, C, D] {
      def modifyFAndOptError[F[+ _] : Applicative](f: C => F[D])(from: S): TraversalRes[F, E1, T] = {
        val res = self.modifyFAndOptError[[+X] =>> TraversalRes[F, E1, X]](other.modifyFAndOptError(f)(_))(from)
        TraversalRes(
          optError = res.optError.orElse(res.effect.optError),
          effect = res.effect.effect
        )
      }
    }
}



object PTraversal {
  def field2[S, T, A, B](get1: S => A, get2: S => A)(_replace: (B, B) => S => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def modifyFAndOptError[F[+ _] : Applicative](f: A => F[B])(from: S): TraversalRes[F, Nothing, T] =
        TraversalRes(None, Applicative[F].map2(f(get1(from)), f(get2(from)))(_replace(_, _)(from)))
    }

  def pair[A, B]: PTraversal[(A, A), (B, B), A, B] =
    field2[(A, A), (B, B), A, B](_._1, _._2)((b1, b2) => _ => (b1, b2))
}

object Traversal {
  def field2[A, B](get1: A => B, get2: A => B)(_replace: (B, B) => A => A): Traversal[A, B] =
    PTraversal.field2(get1, get2)(_replace)
}
