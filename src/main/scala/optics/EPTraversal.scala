package optics

import optics.internal.{Applicative, Id, Proxy, TraversalRes}

import scala.annotation.alpha

trait EPTraversal[+E, -S, +T, +A, -B] { self =>
  def traversal[F[+_]: Applicative](f: A => F[B])(from: S): TraversalRes[F, E, T]

  def modifyFOrError[F[+_]: Applicative](f: A => F[B])(from: S): Either[E, F[T]] =
    traversal(f)(from) match {
      case TraversalRes.Failure(e, _) => Left(e)
      case TraversalRes.Success(res) => Right(res)
    }

  def modifyOrError(f: A => B): S => Either[E, T] =
    modifyFOrError[Id](f)

  def replaceOrError(to: B): S => Either[E, T] =
    modifyOrError(_ => to)

  def modifyF[F[+_]: Applicative](f: A => F[B])(from: S): F[T] =
    traversal(f)(from).effect

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
      def traversal[F[+ _] : Applicative](f: C => F[D])(from: S): TraversalRes[F, E1, T] =
        self.traversal[[+X] =>> TraversalRes[F, E1, X]](other.traversal(f)(_))(from).flatten
    }

  @alpha("andThenDiscardRight")
  def >>>?[E1, C, D](other: EPTraversal[E1, A, B, C, D]): EPTraversal[E, S, T, C, D] =
    new EPTraversal[E, S, T, C, D] {
      def traversal[F[+ _] : Applicative](f: C => F[D])(from: S): TraversalRes[F, E, T] =
        self.traversal(other.modifyF(f)(_))(from)
    }

  @alpha("andThenDiscardLeft")
  def ?>>>[E1, C, D](other: EPTraversal[E1, A, B, C, D]): EPTraversal[E1, S, T, C, D] =
    new EPTraversal[E1, S, T, C, D] {
      def traversal[F[+ _] : Applicative](f: C => F[D])(from: S): TraversalRes[F, E1, T] =
        self.modifyF[[+X] =>> TraversalRes[F, E1, X]](other.traversal(f)(_))(from)
    }
}



object PTraversal {
  def field2[S, T, A, B](get1: S => A, get2: S => A)(_replace: (B, B) => S => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def traversal[F[+ _] : Applicative](f: A => F[B])(from: S): TraversalRes[F, Nothing, T] =
        TraversalRes.Success(Applicative[F].map2(f(get1(from)), f(get2(from)))(_replace(_, _)(from)))
    }

  def pair[A, B]: PTraversal[(A, A), (B, B), A, B] =
    field2[(A, A), (B, B), A, B](_._1, _._2)((b1, b2) => _ => (b1, b2))

  def list[A, B]: PTraversal[List[A], List[B], A, B] =
    new PTraversal[List[A], List[B], A, B] {
      def traversal[F[+ _] : Applicative](f: A => F[B])(from: List[A]): TraversalRes[F, Nothing, List[B]] =
        TraversalRes.Success(
          Applicative[F].map(
            from.foldLeft(Applicative[F].pure(List.empty[B]))((acc, a) =>
              Applicative[F].map2(acc, f(a))((tail, head) => head :: tail)
            )
          )(_.reverse)
        )
    }
}

object Traversal {
  def field2[A, B](get1: A => B, get2: A => B)(_replace: (B, B) => A => A): Traversal[A, B] =
    PTraversal.field2(get1, get2)(_replace)
}
