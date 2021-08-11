package optics.poly

import optics.internal.{Applicative, Id, Proxy}
import optics.poly.functions.Index

trait PTraversal[S, T, A, B] { self =>
  def modifyF[F[_]: Applicative](f: A => F[B])(from: S): F[T]

  def modify(f: A => B): S => T =
    modifyF[Id](f)

  def replace(to: B): S => T =
    modify(_ => to)

  def toList(from: S): List[A] = {
    var acc = List.empty[A]
    modifyF{ a =>
      acc = a :: acc
      Proxy()
    }(from)
    acc.reverse
  }

  def andThen[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] =
    new PTraversal[S, T, C, D] {
      def modifyF[F[_]: Applicative](f: C => F[D])(from: S): F[T] =
        self.modifyF(other.modifyF(f)(_))(from)
    }

  def andThen[C, D](other: POptional[A, B, C, D]): PTraversal[S, T, C, D] = andThen(other.asTraversal)
  def andThen[C, D](other: PPrism[A, B, C, D]): PTraversal[S, T, C, D] = andThen(other.asTraversal)
  def andThen[C, D](other: PLens[A, B, C, D]): PTraversal[S, T, C, D] = andThen(other.asTraversal)
  def andThen[C, D](other: PIso[A, B, C, D]): PTraversal[S, T, C, D] = andThen(other.asTraversal)
}

object PTraversal {
  def field2[S, T, A, B](get1: S => A, get2: S => A)(_replace: (B, B) => S => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {

      def modifyF[F[_] : Applicative](f: A => F[B])(from: S): F[T] =
        Applicative[F].map2(
          f(get1(from)),
          f(get2(from))
        )(_replace(_, _)(from))
    }

  def pair[A, B]: PTraversal[(A, A), (B, B), A, B] =
    field2[(A, A), (B, B), A, B](_._1, _._2)((b1, b2) => _ => (b1, b2))

  def list[A, B]: PTraversal[List[A], List[B], A, B] =
    new PTraversal[List[A], List[B], A, B] {
      def modifyF[F[_] : Applicative](f: A => F[B])(from: List[A]): F[List[B]] =
        Applicative[F].map(
          from.foldLeft[F[List[B]]](Applicative[F].pure(Nil))((acc, a) =>
            Applicative[F].map2(acc, f(a))((tail, head) => head :: tail)
          )
        )(_.reverse)
    }

}

object Traversal {
  def field2[From, To](get1: From => To, get2: From => To)(_replace: (To, To) => From => From): Traversal[From, To] =
    PTraversal.field2(get1, get2)(_replace)

  def pair[A]: Traversal[(A, A), A] = PTraversal.pair

  def list[A]: Traversal[List[A],  A] =  PTraversal.list

}
