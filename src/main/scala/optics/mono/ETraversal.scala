package optics.mono

import optics.internal.{Applicative, Id, TraversalRes}

import scala.annotation.alpha

trait ETraversal[+Error, From, To] { self =>
  def traversal[F[+_]: Applicative, NewError >: Error](f: To => TraversalRes[F, NewError, To])(from: From): TraversalRes[F, NewError, From]

  def modifyFOrError[F[+_]: Applicative](f: To => F[To])(from: From): Either[Error, F[From]] =
    traversal(to => TraversalRes.Success(f(to)))(from) match {
      case TraversalRes.Failure(e, _)   => Left(e)
      case TraversalRes.Success(effect) => Right(effect)
    }

  def modifyOrError(f: To => To): From => Either[Error, From] =
    modifyFOrError[Id](f)

  def replaceOrError(to: To): From => Either[Error, From] =
    modifyOrError(_ => to)

  def modifyF[F[+_]: Applicative](f: To => F[To])(from: From): F[From] =
    modifyFOrError(f)(from).getOrElse(Applicative[F].pure(from))

  def modify(f: To => To): From => From =
    modifyF[Id](f)

  def replace(to: To): From => From =
    modify(_ => to)

  def toListOrError(from: From): Either[Error, List[To]] = {
    var acc = List.empty[To]
    modifyOrError{ a => acc = a :: acc; a }(from).map(_ => acc.reverse)
  }

  def toList(from: From): List[To] =
    toListOrError(from).getOrElse(Nil)

  def iterator(from: From): Iterator[To] =
    toListOrError(from).fold(_ => Iterator.empty, _.iterator)

  @alpha("andThen")
  def >>>[NewError >: Error, Next](other: ETraversal[NewError, To, Next]): ETraversal[NewError, From, Next] =
    new ETraversal[NewError, From, Next] {
      def traversal[F[+_] : Applicative, E1 >: NewError](f: Next => TraversalRes[F, E1, Next])(from: From): TraversalRes[F, E1, From] =
        self.traversal(other.traversal(f)(_))(from)
    }
}

object Traversal {
  def list[A, B]: Traversal[List[A], A] =
    new Traversal[List[A],  A] {
      def traversal[F[+_]: Applicative, NewError >: Any](f: A => TraversalRes[F, NewError, A])(from: List[A]): TraversalRes[F, NewError, List[A]] =
        from.foldLeft[TraversalRes[F, NewError, List[A]]](TraversalRes.Failure("List is empty", Applicative[F].pure(Nil)))((acc, a) =>
          acc.map2Permissive(f(a))((tail, head) => head :: tail)
        ).map(_.reverse)
    }
}

object NonEmptyTraversal {
  def field2[From, To](get1: From => To, get2: From => To, _replace: (To, To) => From => From): NonEmptyTraversal[From, To] =
    new NonEmptyTraversal[From, To] {
      def traversal[F[+_] : Applicative, NewError >: Nothing](f: To => TraversalRes[F, NewError, To])(from: From): TraversalRes[F, NewError, From] =
        f(get1(from)).map2(f(get2(from)))(_replace(_, _)(from))
    }

  def pair[A, B]: NonEmptyTraversal[(A, A), A] =
    field2[(A, A), A](_._1, _._2, (b1, b2) => _ => (b1, b2))
}
